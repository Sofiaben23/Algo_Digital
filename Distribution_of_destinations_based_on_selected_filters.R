library(shiny)
library(dplyr)
library(plotly)
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

# Calculate top countries
get_top_countries <- function(data, N = 10) {
  data %>%
    group_by(Pays) %>%
    summarise(Count = n(), .groups = 'drop') %>%
    top_n(N, Count) %>%
    pull(Pays)
}
top_countries <- get_top_countries(processed_data)

ui <- fluidPage(
  titlePanel("Distribution of Destinations Based on Selected Filters"),
  sidebarLayout(
    sidebarPanel(
      selectInput("filterSelect", "Choose Filter:",
                  choices = c("Pays" = "Pays", "Etoiles" = "Etoiles", "Type" = "Type", "Top Countries" = "top_countries"),
                  selected = "Pays"),
      selectInput("valueSelect", "Choose Value:",
                  choices = NULL),  # Dynamic choices set in server
      selectInput("yearInput", "Select Year:",
                  choices = sort(unique(processed_data$Year), decreasing = TRUE),
                  selected = sort(unique(processed_data$Year), decreasing = TRUE)[1]),
      selectInput("monthInput", "Select Month:",
                  choices = sort(unique(processed_data$Month), decreasing = FALSE),
                  selected = sort(unique(processed_data$Month), decreasing = FALSE)[1])
    ),
    mainPanel(
      plotlyOutput("destinationPlot")
    )
  )
)

server <- function(input, output, session) {
  # Dynamically update the choices of valueSelect based on filterSelect
  observe({
    current_filter <- input$filterSelect
    if(current_filter == "top_countries") {
      updateSelectInput(session, "valueSelect",
                        choices = top_countries)
    } else {
      updateSelectInput(session, "valueSelect",
                        choices = sort(unique(processed_data[[current_filter]])))
    }
  })
  
  output$destinationPlot <- renderPlotly({
    # Adjusting the filtering logic to accommodate selected filter value
    # Check if the selected filter is 'top_countries' and adjust the filter column accordingly
    filter_column <- if(input$filterSelect == "top_countries") "Pays" else input$filterSelect
    
    filtered_data <- processed_data %>%
      filter((.data[[filter_column]] == input$valueSelect) &
               Year == input$yearInput &
               Month == input$monthInput)
    
    # Calculate the counts and percentages
    destination_counts <- filtered_data %>%
      count(Destination) %>%
      mutate(Total = sum(n), Percentage = n / Total * 100) %>%
      arrange(desc(n))
    
    custom_colors <- rainbow(nrow(destination_counts))
    
    # Create the plot with updated hovertext to include percentages
    plot_ly(destination_counts,
            x = ~Destination,
            y = ~n,
            type = 'bar',
            marker = list(color = custom_colors),
            hoverinfo = 'text',
            hovertext = ~paste("Count: ", n, "<br>Percentage: ", round(Percentage, 2), "%")) %>%
      layout(title = "Distribution of Destinations",
             xaxis = list(title = "Destination"),
             yaxis = list(title = "Count"))
  })
}

# Run the application
shinyApp(ui, server)
