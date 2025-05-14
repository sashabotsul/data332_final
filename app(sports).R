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


data_url <- getURL('https://raw.githubusercontent.com/sashabotsul/data332_final/refs/heads/main/data/combined_data_with_teams.csv')
league_data <- read.csv(text = data_url)


league_data$Year <- as.numeric(league_data$Year)
column_names<-colnames(league_data)


adjust_for_inflation <- function(league_data, inflation_rate = league_data$Inflation_Rate) {
  league_data %>% mutate(adj_salary = mean_salary * (1 + inflation_rate)^(max(Year) - Year))
}




ui <- fluidPage(
  theme = shinythemes::shinytheme('superhero'),

  navset_card_underline(
    header = h1('Sports Salaries'),
    
    nav_panel('Our Project',
              h2('Our Project'),
              wellPanel(
              tags$p('For this project, we have chosen to analyze different sports
              salaries, comparing them within sports and between other sports.', style = "font-size: 18px;")),
              h3('Our Research'),
              wellPanel(
                tags$p('We chose to look at different sport salaries, starting in 1985.
                   The sports we are looking at are baseball and basketball.
                   The reason we chose to look at these salaries is because these are 2 
                   of the more popular sports in the United States. We thought it would
                   be interesting to see how different sport salaries compare and if there
                   would be a trend in the data when adjusting the growth in salaries to 
                   inflation. We are interested to see if a more popular sport among the 2
                   might have a higher salary, which we believe will be basketball.', 
                       style = "font-size: 18px;")),
              h3('Scope of Project'),
              wellPanel(
                tags$p('The scope of our project is as follows:', style = "font-size: 18px;"),
                tags$p('Compare salaries of sports (baseball and basketball) to each other and
                   within each sport, as well as the salaries adjusted for inflation.', style = "font-size: 18px;")),
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
                tags$p('We ran into several conflicts with our plans during our project.
                   Originally, we wanted to include more sports, including football,
                   hockey, and soccer. When we started, it was clear hockey and soccer
                   were not available. After looking for football in many different places,
                   it was too hard to come across. While we could have scraped websites with
                   data that could have been used, it would have violated the terms of service.
                   Additionally, we were hoping to compare our salary information to the
                   viewership trends of the sports. Once again, this data was not readily 
                   available for all of the sports or for more than just world championships.',
                   style = "font-size: 18px;"))
              ),
  
  
  nav_panel('Salary Charts',
            h2('Salary Trends'),
            selectInput("selected_league", "Choose a League:", choices = NULL),
            sliderInput("year_range", "Select Year Range:", min = 1985, max = 2025, value = c(2000, 2024), sep = ""),
            selectInput("inflation_adjustment", "Adjust for Inflation:", choices = c("No", "Yes")),
            plotOutput("salaryTrendPlot"),
            wellPanel(h5('description of chart'))
            ),
            
            
  
  nav_panel('Team Salary Heatmap',
            h2('Team-Year Salary Heatmap'),
            selectInput("selected_league", "Choose a League:", choices = c('MLB', 'NBA')),
            sliderInput("year_range", "Select Year Range:", min = 1985, max = 2025, value = c(2000, 2024), sep = ""),
            selectInput("inflation_adjustment", "Adjust for Inflation:", choices = c("No", "Yes")),
            plotOutput('salaryHeatmap'),
            wellPanel(h5('description of chart'))
            
            ),
  nav_panel('Salary growth between teams', 
            h2('Salary growth between teams'),
            fluidRow(
    column(3, 
           selectInput('salary_metric', 'Choose Salary Metric:',
                       choices = c('Average Salary', 'Median Salary')),
           selectInput("selected_league", "Choose a League:", choices = c('MLB', 'NBA')),
    ),
    
    column(9,
           plotOutput('salary_by_year_plot', height = "1200px", width = "1000px"),
           wellPanel(h5(''))
           )
  )
  )
  )
)

  
  


server<- function(input, output, session) {
  
  observe({
    updateSelectInput(session, "selected_league", choices = unique(league_data$sport), selected = unique(league_data$sport)[1])
  })
  
  filtered_data <- reactive({
    league_data %>%
      filter(sport == input$selected_league,
             Year >= input$year_range[1],
             Year <= input$year_range[2])
  })
  
  adjusted_data <- reactive({
    df <- filtered_data()
    if (input$inflation_adjustment == "Yes") {
      adjust_for_inflation(df)
    } else {
      df %>% mutate(adj_salary = mean_salary)
    }
  })
  

  #Chart1
  output$salaryTrendPlot <- renderPlot({
    df <- adjusted_data()
    if (nrow(df) == 0) return(NULL)
    df %>%
      group_by(Year) %>%
      summarise(mean_salary = mean(adj_salary, na.rm = TRUE)) %>%
      ggplot(aes(x = Year, y = mean_salary)) +
      geom_line(color = "steelblue", linewidth = 1.2) +
      labs(title = paste("Average Salary Over Time -", input$selected_league),
           x = "Year", y = "Average Salary") +
      scale_y_continuous(labels = label_comma()) +
      theme_minimal()
  })
  
  #Chart2
  output$salaryHeatmap <- renderPlot({
    df <- adjusted_data()
    if (nrow(df) == 0) return(NULL)
    ggplot(df, aes(x = teamID, y = factor(Year), fill = adj_salary)) +
      geom_tile(color = "white") +
      scale_fill_gradient(low = "lightblue", high = "darkblue", name = "Avg Salary", labels = label_comma()) +
      labs(title = paste("Average Salary Heatmap -", input$selected_league),
           x = "Team", y = "Year") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  })  
  #Chart3
  output$salary_by_year_plot <- renderPlot({
    req(input$salary_metric, input$selected_league)
    
    df <- adjusted_data() 
    if (nrow(df) == 0) return(NULL)
    
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
      facet_wrap(~teamID, scales = 'free', ncol = 4)+
      labs(title = paste(input$salary_metric, "by Team for", input$selected_league),
           x = "Year", y = input$salary_metric) +
      scale_y_continuous(labels = label_comma())+
      theme_minimal()+
      theme(legend.position = "none")
    
  })
  
  
  
}


shinyApp(ui=ui, server=server)
