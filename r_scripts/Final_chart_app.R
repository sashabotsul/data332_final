library(shiny)
library(ggplot2)
library(dplyr)
library(tidyr)
library(scales)
library(RCurl)
library(DT)
library(readxl)
library(bslib)
library(hms)
library(lubridate)
library(viridis)
library(leaflet)
library(leaflet.extras)

rm(list = ls())

# ---- Data Loading and Preprocessing ----

# Load MLB Data
mlb_1985_2012_url <- getURL('https://raw.githubusercontent.com/sashabotsul/data332_final/refs/heads/main/data/MLB_Salaries_1985_2012.csv')
df_mlb_1985_2012 <- read.csv(text = mlb_1985_2012_url) %>%
  rename(Year = yearID) %>%
  select(-c(playerID, lgID))

mlb_2011_2024_url <- getURL('https://raw.githubusercontent.com/sashabotsul/data332_final/refs/heads/main/data/mlb_salary_data_2011_2024.csv')
df_mlb_2011_2024 <- read.csv(text = mlb_2011_2024_url) %>%
  filter(!(Year %in% c('2011', '2012'))) %>%
  rename(teamID = Team, salary = Salary) %>%
  select(-c(Name))

df_mlb_1985_2024 <- bind_rows(df_mlb_1985_2012, df_mlb_2011_2024)

# Fix team names
replacement_map <- c("ARI" = "AZ", "CAL" = "LAA", "CHA" = "CWS", "CHN" = "CHC",
                     "CHW" = "CWS", "FLO" = "FLA", "KCA" = "KC", "LAN" = "LAD",
                     "ML4" = "MIL", "MON" = "MTL", "NYA" = "NYY", "NYN" = "NYM",
                     "SDN" = "SD", "SFN" = "SF", "SLN" = "STL", "WAS" = "WSH",
                     "TBA" = "TB", "ANA" = "LAA", "MTL" = "WSH", "FLA" = "MIA")

df_mlb_1985_2024 <- df_mlb_1985_2024 %>%
  mutate(teamID = ifelse(teamID %in% names(replacement_map), replacement_map[teamID], teamID))

df_mlb_summary <- df_mlb_1985_2024 %>%
  group_by(Year, teamID) %>%
  summarise(avg_salary = mean(salary, na.rm = TRUE), .groups = 'drop') %>%
  mutate(league = "MLB") %>%
  rename(year = Year)

# NBA Data
NBA_1984_2018_url <- getURL('https://raw.githubusercontent.com/sashabotsul/data332_final/refs/heads/main/data/NBA_Salaries_1985to2018.csv')
df_NBA_1984_2018 <- read.csv(text = NBA_1984_2018_url) %>%
  select(-c(season, season_end, player_id, league)) %>%
  rename(year = season_start, teamID = team) %>%
  mutate(teamID = na_if(teamID, "")) %>%
  drop_na()

team_replacement_map <- c(
  "Charlotte Bobcats" = "Charlotte Hornets",
  "Kansas City Kings" = "Sacramento Kings",
  "New Jersey Nets" = "Brooklyn Nets",
  "New Orleans Hornets" = "New Orleans Pelicans",
  "New Orleans/Oklahoma City Hornets" = "New Orleans Pelicans",
  "Seattle SuperSonics" = "Oklahoma City Thunder",
  "Vancouver Grizzlies" = "Memphis Grizzlies",
  "Washington Bullets" = "Washington Wizards"
)

df_NBA_1984_2018 <- df_NBA_1984_2018 %>%
  mutate(teamID = ifelse(teamID %in% names(team_replacement_map), team_replacement_map[teamID], teamID))

df_nba_summary <- df_NBA_1984_2018 %>%
  group_by(year, teamID) %>%
  summarise(avg_salary = mean(salary, na.rm = TRUE), .groups = 'drop') %>%
  mutate(league = "NBA")

# Combine
league_data <- bind_rows(df_mlb_summary, df_nba_summary)

adjust_for_inflation <- function(data, inflation_rate = 0.02) {
  data %>% mutate(adj_salary = avg_salary * (1 + inflation_rate)^(max(year) - year))
}

# ---- UI ----
ui <- fluidPage(
  theme = shinythemes::shinytheme("superhero"),
  navset_card_underline(
    header = h1('Sports Salaries'),
    
    nav_panel('Our Project',
              h2('Our Project'),
              wellPanel(tags$p('For this project, we have chosen to analyze different sports salaries...', style = "font-size: 18px;")),
              h3('Our Research'),
              wellPanel(tags$p('We chose to look at different sport salaries, starting in 1985...', style = "font-size: 18px;")),
              h3('Scope of Project'),
              wellPanel(tags$p('Compare salaries of sports...', style = "font-size: 18px;")),
              h3('Requirements of Project'),
              wellPanel(tags$ul(
                tags$li("Clean the data of any unnecessary columns", style = "font-size: 18px;"),
                tags$li("Adjust salaries for inflation", style = "font-size: 18px;"),
                tags$li("Compare salary growth to inflation growth", style = "font-size: 18px;"),
                tags$li("Compare sport salary trend lines", style = "font-size: 18px;"),
                tags$li("Compare individual salaries within a sport", style = "font-size: 18px;"))),
              h3('Ideas and Original Plans'),
              wellPanel(tags$p('We ran into several conflicts with our plans during our project...', style = "font-size: 18px;"))
    ),
    
    nav_panel('Salary Charts',
              h3('Salary Trend'),
              selectInput("selected_league", "Choose a League:", choices = NULL),
              sliderInput("year_range", "Select Year Range:", min = 1985, max = 2025, value = c(2000, 2024), sep = ""),
              selectInput("inflation_adjustment", "Adjust for Inflation:", choices = c("No", "Yes")),
              plotOutput("salaryTrendPlot")
    ),
    
    nav_panel('Team Salary Heatmap',
              h3('Team-Year Salary Heatmap'),
              plotOutput("salaryHeatmap")
    )
  )
)

# ---- Server ----
server <- function(input, output, session) {
  observe({
    updateSelectInput(session, "selected_league", choices = unique(league_data$league), selected = unique(league_data$league)[1])
  })
  
  filtered_data <- reactive({
    league_data %>%
      filter(league == input$selected_league,
             year >= input$year_range[1],
             year <= input$year_range[2])
  })
  
  adjusted_data <- reactive({
    df <- filtered_data()
    if (input$inflation_adjustment == "Yes") {
      adjust_for_inflation(df)
    } else {
      df %>% mutate(adj_salary = avg_salary)
    }
  })
  
  output$salaryTrendPlot <- renderPlot({
    df <- adjusted_data()
    if (nrow(df) == 0) return(NULL)
    df %>%
      group_by(year) %>%
      summarise(mean_salary = mean(adj_salary, na.rm = TRUE)) %>%
      ggplot(aes(x = year, y = mean_salary)) +
      geom_line(color = "steelblue", size = 1.2) +
      labs(title = paste("Average Salary Over Time -", input$selected_league),
           x = "Year", y = "Average Salary") +
      theme_minimal()
  })
  
  output$salaryHeatmap <- renderPlot({
    df <- adjusted_data()
    if (nrow(df) == 0) return(NULL)
    ggplot(df, aes(x = teamID, y = factor(year), fill = adj_salary)) +
      geom_tile(color = "white") +
      scale_fill_gradient(low = "lightblue", high = "darkblue", name = "Avg Salary") +
      labs(title = paste("Average Salary Heatmap -", input$selected_league),
           x = "Team", y = "Year") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  })
}

shinyApp(ui = ui, server = server)
