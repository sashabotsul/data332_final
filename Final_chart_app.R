library(shiny)
library(ggplot2)
library(dplyr)
library(tidyr)
library(scales)

# Example inflation adjustment function (a simple inflation index, customize as needed)
adjust_for_inflation <- function(data, inflation_rate = 0.02) {
  data %>%
    mutate(
      adj_salary = avg_salary * (1 + inflation_rate)^(max(year) - year)
    )
}

# Assume league_data is preloaded and cleaned
# Columns: year, league, avg_salary, avg_viewership

ui <- fluidPage(
  titlePanel("Sports League Salaries and Viewership Over Time"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("selected_league", "Choose a League:", choices = NULL, selected = NULL),
      sliderInput("year_range", "Select Year Range:",
                  min = 2000, max = 2025,
                  value = c(2010, 2025), sep = ""),
      selectInput("inflation_adjustment", "Adjust for Inflation:", choices = c("No", "Yes"), selected = "No")
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("Salary vs. Viewership (Dual Axis)", plotOutput("combinedTrendPlot")),
        tabPanel("Normalized Comparison", plotOutput("normalizedPlot"))
      )
    )
  )
)

server <- function(input, output, session) {
  # Update league choices dynamically
  observe({
    updateSelectInput(session, "selected_league",
                      choices = unique(league_data$league),
                      selected = unique(league_data$league)[1])
  })
  
  # Filter data based on inputs
  filtered_data <- reactive({
    league_data %>%
      filter(league == input$selected_league,
             year >= input$year_range[1],
             year <= input$year_range[2])
  })
  
  # Adjust salary for inflation if selected
  adjusted_data <- reactive({
    data <- filtered_data()
    if (input$inflation_adjustment == "Yes") {
      adjust_for_inflation(data, inflation_rate = 0.02)  # You can adjust inflation rate here
    } else {
      data
    }
  })
  
  # Plot with dual y-axes
  output$combinedTrendPlot <- renderPlot({
    df <- adjusted_data()
    
    if (nrow(df) == 0) return(NULL)
    
    scale_factor <- max(df$avg_salary, na.rm = TRUE) / max(df$avg_viewership, na.rm = TRUE)
    
    ggplot(df, aes(x = year)) +
      geom_line(aes(y = adj_salary, color = "Adjusted Salary"), size = 1.2) +  # Adjusted Salary if inflation is applied
      geom_line(aes(y = avg_viewership * scale_factor, color = "Average Viewership"), size = 1.2) +
      scale_y_continuous(
        name = "Adjusted Salary (USD)",
        sec.axis = sec_axis(~ . / scale_factor, name = "Average Viewership")
      ) +
      labs(
        title = paste("Salaries vs. Viewership Over Time -", input$selected_league),
        x = "Year",
        color = "Metric"
      ) +
      theme_minimal()
  })
  
  # Normalized comparison plot
  output$normalizedPlot <- renderPlot({
    df <- adjusted_data()
    
    if (nrow(df) == 0) return(NULL)
    
    df_norm <- df %>%
      mutate(
        salary_scaled = (adj_salary - min(adj_salary)) / (max(adj_salary) - min(adj_salary)),  # Use adjusted salary
        viewership_scaled = (avg_viewership - min(avg_viewership)) / (max(avg_viewership) - min(avg_viewership))
      ) %>%
      pivot_longer(cols = c(salary_scaled, viewership_scaled),
                   names_to = "metric", values_to = "value")
    
    ggplot(df_norm, aes(x = year, y = value, color = metric)) +
      geom_line(size = 1.2) +
      labs(
        title = paste("Normalized Trends in Salaries and Viewership -", input$selected_league),
        x = "Year",
        y = "Normalized Value",
        color = "Metric"
      ) +
      theme_minimal()
  })
}

shinyApp(ui = ui, server = server)
