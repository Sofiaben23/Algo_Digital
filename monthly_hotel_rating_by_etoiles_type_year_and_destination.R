library(shiny)
library(plotly)
library(dplyr)
library(readr)
library(lubridate)  # Ensure lubridate is installed for date manipulation

# Load the data
processed_data <- read_delim("./Data&Shiny/2024.03_Data_Booking.com_VD_processed_joint.csv", 
                             delim = ";", trim_ws = TRUE)

# Prepare the data
processed_data$Date <- dmy(processed_data$Date)
processed_data <- processed_data %>%
  mutate(
    Year = year(Date),
    Month = month(Date, label = TRUE, abbr = FALSE)  # Month as full name
  )

# Define custom colors
custom_colors <- c("#1f77b4", "#ff7f0e", "#2ca02c", "#d62728", "#9467bd", "#8c564b")

# Get top 10 countries for each year
get_top_countries <- function(data, N = 10) {
  data %>%
    group_by(Pays) %>%
    summarise(Count = n(), .groups = 'drop') %>%
    top_n(N, Count) %>%
    pull(Pays)
}

# Calculate the top countries only once to optimize performance
top_countries <- get_top_countries(processed_data)

ui <- fluidPage(
  titlePanel("Dynamic Hotel Ratings by Month"),
  sidebarLayout(
    sidebarPanel(
      selectInput("categoryInput", "Choose Category:",
                  choices = c("Etoiles" = "Etoiles", "Type" = "Type", "Top Countries" = "top_countries"),
                  selected = "Etoiles"),
      selectInput("destinationInput", "Choose Destinations:",
                  choices = unique(processed_data$Destination),
                  selected = unique(processed_data$Destination)[1]),
      selectInput("yearInput", "Select Year:",
                  choices = sort(unique(processed_data$Year), decreasing = TRUE),
                  selected = sort(unique(processed_data$Year), decreasing = TRUE)[1])
    ),
    mainPanel(
      plotlyOutput("ratingsPlot")
    )
  )
)

server <- function(input, output) {
  output$ratingsPlot <- renderPlotly({
    # Filter data based on selected year and destinations
    filtered_data <- processed_data %>%
      filter(Destination %in% input$destinationInput,
             Year == input$yearInput)
    
    # Apply dynamic grouping based on selected category
    if (input$categoryInput == "top_countries") {
      filtered_data <- filtered_data %>% 
        filter(Pays %in% top_countries)  # Filter for top countries only
    }
    
    category <- ifelse(input$categoryInput == "top_countries", "Pays", input$categoryInput)
    
    aggregated_data <- filtered_data %>%
      group_by(Month, .data[[category]]) %>%
      summarise(Count = n(), .groups = 'drop') %>%
      mutate(Total = sum(Count),
             Percentage = Count / Total * 100)
    
    # Create bar chart
    p <- plot_ly(aggregated_data, x = ~Month, y = ~Count, color = ~.data[[category]],
                 colors = custom_colors, type = 'bar',
                 text = ~paste("Count:", Count, "<br>Percentage:", round(Percentage, 2), "%"),
                 hoverinfo = "text+x+y", split = ~.data[[category]]) %>%
      layout(title = paste("Monthly Distribution by", category),
             xaxis = list(title = "Month"),
             yaxis = list(title = "Number of Ratings"),
             barmode = 'stack',
             legend = list(x = 1.05, xanchor = 'left', y = 1))
    
    return(p)
  })
}

# Run the application
shinyApp(ui, server)
