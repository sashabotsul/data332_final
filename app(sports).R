library(shiny)
library(ggplot2)
library(DT)
library(readxl)
library(RCurl)
library(bslib)
library(dplyr)
library(tidyr)
library(hms)
library(lubridate)
library(viridis)
library(scales)
rm(list=ls())

# Load the data
data_url <- getURL('https://raw.githubusercontent.com/sashabotsul/data332_final/refs/heads/main/data/combined_data_with_teams.csv')
league_data <- read.csv(text = data_url)

league_data$Year <- as.numeric(league_data$Year)

# UI Definition
ui <- fluidPage(
  theme = shinythemes::shinytheme('superhero'),
  
  # Header for the app
  navset_card_underline(
    header = h1('Sports Salaries'),
    
    # Our Project Panel
    nav_panel('Our Project',
              h2('Our Project'),
              wellPanel(
                tags$p('For this project, we have chosen to analyze different sports salaries, comparing them within sports and between other sports.', style = "font-size: 18px;")),
              h3('Our Research'),
              wellPanel(
                tags$p('We chose to look at different sport salaries, starting in 1985. The sports we are looking at are baseball and basketball. We are interested to see if a more popular sport might have a higher salary, which we believe will be basketball.',
                       style = "font-size: 18px;")),
              h3('Scope of Project'),
              wellPanel(
                tags$p('The scope of our project is to compare salaries of sports (baseball and basketball) to each other and within each sport, as well as the salaries adjusted for inflation.', style = "font-size: 18px;")),
              h3('Requirements of Project'),
              wellPanel(
                tags$p("The requirements we have set for our project include:", style = "font-size: 18px;"),
                tags$ul(
                  tags$li("Clean the data of any unnecessary columns", style = "font-size: 18px;"),
                  tags$li("Adjust salaries for inflation", style = "font-size: 18px;"),
                  tags$li("Compare salary growth to inflation growth", style = "font-size: 18px;"),
                  tags$li("Compare sport salary trend lines", style = "font-size: 18px;"),
                  tags$li("Compare individual salaries within a sport", style = "font-size: 18px;")
                )
              ),
              h3('Ideas and Original Plans'),
              wellPanel(
                tags$p('We ran into several conflicts with our plans during our project. Originally, we wanted to include more sports, including football, hockey, and soccer. Unfortunately, the data for these was unavailable, and scraping violated terms of service. We also wanted to compare salaries to viewership trends, but that data was limited.', style = "font-size: 18px;"))
    ),
    
    # Salary Charts Panel
    nav_panel('Salary Charts',
              h2('Salary Trends'),
              selectInput("selected_league_trend", "Choose League(s):", choices = c('MLB', 'NBA'), multiple = TRUE, selected = 'MLB'),
              sliderInput("year_range_trend", "Select Year Range:", min = 1985, max = 2025, value = c(2000, 2024), sep = ""),
              plotOutput("salaryTrendPlot"),
              wellPanel(h5('This chart displays the average salary trend over time for the selected league, based on the chosen year range.'))
    ),
    
    # Salary vs Inflation Panel
    nav_panel('Salary vs Inflation',
              h2('Salary Growth Trend with Inflation Growth'),
              selectInput("selected_league_inflation", "Choose League(s):", choices = c('MLB', 'NBA'), selected = 'MLB', multiple = TRUE),
              sliderInput("year_range_inflation", "Select Year Range:", min = 1985, max = 2025, value = c(2000, 2024), sep = ""),
              selectInput('selected_team_inflation', 'Choose a Team:', choices = NULL),
              plotOutput('salary_growth_plot'),
              wellPanel(h5('This chart compares the percentage change in salary versus inflation growth for the selected team and league.'))
    ),
    
    # Team Salary Heatmap Panel
    nav_panel('Team Salary Heatmap',
              h2('Team-Year Salary Heatmap'),
              selectInput("selected_league_heatmap", "Choose a League:", choices = c('MLB', 'NBA')),
              sliderInput("year_range_heatmap", "Select Year Range:", min = 1985, max = 2025, value = c(2000, 2024), sep = ""),
              plotOutput('salaryHeatmap'),
              wellPanel(h5('This heatmap visualizes the average salary trend over time for the chosen league by team, based on the chosen year range.'))
    ),
    
    # Salary Trend by Teams Panel
    nav_panel('Salary Trend by Teams', 
              h2('Salary Trend by Teams'),
              fluidRow(
                column(3, 
                       selectInput('salary_metric', 'Choose Salary Metric:', choices = c('Average Salary', 'Median Salary')),
                       selectInput("selected_league_teams", "Choose a League:", choices = c('MLB', 'NBA'))
                ),
                column(9,
                       plotOutput('salary_by_year_plot', height = "1200px", width = "1000px"),
                       wellPanel(h5("This chart shows the salary growth trend for each team in the selected league. Each panel displays a team's average or median salary over time, with years shown on the x-axis."))
                )
              )
    )
  )
)

# Server Function
server <- function(input, output, session) {
  
  # Update team selection based on league choice for "Salary vs Inflation" panel
  observeEvent(input$selected_league_inflation, {
    team_choices <- c('All Teams', unique(league_data$teamID[league_data$sport %in% input$selected_league_inflation]))
    updateSelectInput(session, 'selected_team_inflation', choices = team_choices, selected = team_choices[1])
  })
  
  # Filter data for "Salary Charts" panel (Salary Trend)
  filtered_data_trend <- reactive({
    req(input$selected_league_trend)
    league_data %>%
      filter(sport %in% input$selected_league_trend,
             Year >= input$year_range_trend[1],
             Year <= input$year_range_trend[2])
  })
  
  
  # Filter data for "Salary vs Inflation" panel
  filtered_data_inflation <- reactive({
    req(input$selected_league_inflation)
    df <- league_data %>%
      filter(sport == input$selected_league_inflation,
             Year >= input$year_range_inflation[1],
             Year <= input$year_range_inflation[2])
    if (input$selected_team_inflation != 'All Teams'){
      df <- df %>% filter(teamID == input$selected_team_inflation)
    }
    return(df)
  })
  
  # Filter data for "Salary Heatmap" panel
  filtered_data_heatmap <- reactive({
    req(input$selected_league_heatmap)
    df <- league_data %>%
      filter(sport == input$selected_league_heatmap,
             Year >= input$year_range_heatmap[1],
             Year <= input$year_range_heatmap[2])
    return(df)
  })
  
  # Filter data for "Salary Trend by Teams" panel
  filtered_data_teams <- reactive({
    req(input$selected_league_teams)
    df <- league_data %>%
      filter(sport == input$selected_league_teams,
             Year >= input$year_range_heatmap[1],
             Year <= input$year_range_heatmap[2])
    return(df)
  })
  
  # Salary Trend Plot
  output$salaryTrendPlot <- renderPlot({
    df <- filtered_data_trend()
    req(df)
    
    df %>%
      group_by(Year, sport) %>%
      summarise(mean_salary = mean(mean_salary, na.rm = TRUE), .groups = 'drop') %>%
      ggplot(aes(x = Year, y = mean_salary, color = sport)) +
      geom_line(linewidth = 1.2) +
      labs(
        title = "Average Salary Over Time",
        x = "Year", y = "Average Salary",
        color = "League"
      ) +
      scale_y_continuous(labels = label_comma()) +
      theme_minimal()
  })
  
  
  # Salary Growth vs Inflation Plot
  output$salary_growth_plot <- renderPlot({
    df <- filtered_data_inflation()
    req(df)
    
    # Compute salary growth and inflation rate per league
    salary_summary <- df %>%
      group_by(sport, Year) %>%
      summarise(
        avg_salary = mean(mean_salary, na.rm = TRUE),
        inflation = mean(Inflation_Rate, na.rm = TRUE),
        .groups = 'drop'
      ) %>%
      arrange(sport, Year) %>%
      group_by(sport) %>%
      mutate(
        salary_growth = (avg_salary - lag(avg_salary)) / lag(avg_salary),
        inflation_pct = inflation / 100
      ) %>%
      filter(!is.na(salary_growth)) %>%
      ungroup()
    
    # Melt the data for plotting
    plot_data <- salary_summary %>%
      pivot_longer(cols = c("salary_growth", "inflation_pct"),
                   names_to = "metric", values_to = "value") %>%
      mutate(metric = recode(metric,
                             "salary_growth" = "Salary Growth",
                             "inflation_pct" = "Inflation"))
    
    # Plot
    ggplot(plot_data, aes(x = Year, y = value, color = sport, linetype = metric)) +
      geom_line(size = 1.2) +
      scale_y_continuous(labels = scales::percent) +
      labs(
        title = "Salary Growth vs Inflation by League",
        x = "Year",
        y = "Percentage (%)",
        color = "League",
        linetype = "Metric"
      ) +
      theme_minimal()
  })
  
  
  # Salary Heatmap Plot
  output$salaryHeatmap <- renderPlot({
    df <- filtered_data_heatmap()
    req(df)
    ggplot(df, aes(x = teamID, y = factor(Year), fill = mean_salary)) +
      geom_tile(color = "white") +
      scale_fill_gradient(low = "lightblue", high = "darkblue", name = "Avg Salary", labels = label_comma()) +
      labs(title = paste("Average Salary Heatmap -", input$selected_league_heatmap),
           x = "Team", y = "Year") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  })
  
  # Salary by Year Plot by Team
  output$salary_by_year_plot <- renderPlot({
    df <- filtered_data_teams()
    req(df, input$salary_metric)
    
    salary_col <- if (input$salary_metric == "Average Salary") {
      "mean_salary"
    } else {
      "median_salary"
    }
    
    df <- df %>%
      group_by(Year, teamID) %>%
      summarise(salary = mean(.data[[salary_col]], na.rm = TRUE), .groups = "drop")
    
    ggplot(df, aes(x = Year, y = salary, fill = teamID)) +
      geom_line(color = "steelblue", linewidth = 1.2) +
      geom_point() +
      facet_wrap(~teamID, scales = 'free', ncol = 4) +
      labs(title = paste(input$salary_metric, "by Team for", input$selected_league_teams),
           x = "Year", y = input$salary_metric) +
      scale_y_continuous(labels = label_comma()) +
      theme_minimal() +
      theme(legend.position = "none")
  })
}

# Run the Shiny app
shinyApp(ui = ui, server = server)
