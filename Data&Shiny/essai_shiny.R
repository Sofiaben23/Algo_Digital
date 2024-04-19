library(shiny)
library(dplyr)
library(plotly)
library(readr)
library(lubridate)  # Load lubridate for date manipulation

# Use here to construct paths

processed_data <- read_delim("../2024.03_Data_Booking.com_VD_processed_joint.csv", 
                             delim = ";", trim_ws = TRUE)

# Convert Date column to Date format
processed_data <- processed_data %>%
  mutate(Date = dmy(Date),  # Convert the Date column to Date format
         Year = year(Date),   # Extract the year from the Date column
         Month = month(Date, label = TRUE))  # Extract the month from the Date column

ui <- fluidPage(
  selectInput("typeSelect", "Choose Type:", 
              choices = c("All", unique(processed_data$Type))),
  selectInput("destinationSelect", "Choose Destinations:",
              c("All", unique(processed_data$Destination))),
  selectInput("PaysSelect", "Choose Pays:",
              c("All", unique(processed_data$Pays))),
  selectInput("yearSelect", "Choose Year:",
              c("All", unique(processed_data$Year))),
  plotlyOutput("destinationPlot")
)

server <- function(input, output) {
  output$destinationPlot <- renderPlotly({
    # Adjusting the filtering logic to accommodate multiple selections
    filtered_data <- processed_data
    
    if("All" %in% input$typeSelect || length(input$typeSelect) == 0) {
      filtered_data <- processed_data
    } else {
      filtered_data <- processed_data %>% filter(Type %in% input$typeSelect)
    }
    
    if ("All" %in% input$destinationSelect || length(input$destinationSelect) == 0) {
      # Do nothing, keep all destinations
    } else {
      filtered_data <- filtered_data %>% filter(Destination %in% input$destinationSelect)
    }
    
    if ("All" %in% input$PaysSelect || length(input$PaysSelect) == 0) {
      # Do nothing, keep all countries
    } else {
      filtered_data <- filtered_data %>% filter(Pays %in% input$PaysSelect)
    }
    
    if ("All" %in% input$yearSelect || length(input$yearSelect) == 0) {
      # Do nothing, keep all years
    } else {
      filtered_data <- filtered_data %>% filter(Year == input$yearSelect)
    }
    
    # Count the occurrences of each month
    month_counts <- filtered_data %>% 
      count(Month) %>% 
      arrange(Month)
    
    # Generate custom colors for each month
    custom_colors <- rainbow(nrow(month_counts))
    
    # Create the histogram plot
    plot_ly(month_counts, 
            x = ~Month, 
            y = ~n,
            type = 'bar', 
            marker = list(color = custom_colors)) %>%
      layout(title = "Distribution of Bookings by Month",
             xaxis = list(title = "Month"),
             yaxis = list(title = "Count"))
  })
}

shinyApp(ui, server)

