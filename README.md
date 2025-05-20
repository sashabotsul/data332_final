# 🏀 ⚾ Sports Salaries ⚾ 🏀
<p> by: Sasha Botsul, Tanner Buol, Logan Farley </p>

---
## 🎉 Shiny App 🎉 
https://augustanasb4.shinyapps.io/final_sports/
---

### This page shows snippets of our code during the process of cleaning our sport salary data, creating charts and the shiny app.

---
## 🫧 Data Cleaning 🫧
1. Importing MLB data, renaming columns, and removing columns
```
mlb_1985_2012_url <- getURL('https://raw.githubusercontent.com/sashabotsul/data332_final/refs/heads/main/data/MLB_Salaries_1985_2012.csv')
df_mlb_1985_2012 <- read.csv(text = mlb_1985_2012_url)
df_mlb_1985_2012 <- df_mlb_1985_2012 %>% rename(Year = yearID)
df_mlb_1985_2012 <- df_mlb_1985_2012 %>% select(-c(playerID, lgID))

mlb_2011_2024_url <- getURL('https://raw.githubusercontent.com/sashabotsul/data332_final/refs/heads/main/data/mlb_salary_data_2011_2024.csv')
df_mlb_2011_2024 <- read.csv(text = mlb_2011_2024_url)

df_mlb_2011_2024 <- df_mlb_2011_2024 %>% filter(Year != '2011')
df_mlb_2011_2024 <- df_mlb_2011_2024 %>% filter(Year != '2012')
df_mlb_2011_2024 <- df_mlb_2011_2024 %>% rename(teamID = Team)
df_mlb_2011_2024 <- df_mlb_2011_2024 %>% rename(salary = Salary)
df_mlb_2011_2024 <- df_mlb_2011_2024 %>% select(-c(Name))
```

2. Fixing MLB team names and applying to dataframe
```
#fix team names
replacement_map <- c(
  "ARI" = "AZ", "CAL" = "LAA", "CHA" = "CWS", "CHN" = "CHC", "CHW" = "CWS",
  "FLO" = "FLA", "KCA" = "KC", "LAN" = "LAD", "ML4" = "MIL", "MON" = "WSH",
  "NYA" = "NYY", "NYN" = "NYM", "SDN" = "SD", "SFN" = "SF", "SLN" = "STL",
  "WAS" = "WSH", "TBA" = "TB", "ANA" = "LAA", "MTL" = "WSH", "FLA" = "MIA"
)

# Apply mapping to dataframe
df_mlb_1985_2024 <- df_mlb_1985_2024 %>%
  mutate(teamID = ifelse(teamID %in% names(replacement_map), replacement_map[teamID], teamID))
```

3. Creating pivot table with MLB data, to group by year and summarise salary data
```
df_mlb_with_teams <- df_mlb_1985_2024 %>%
  group_by(Year, teamID) %>%
  summarise(
    num_players = n(),
    mean_salary = mean(salary),
    median_salary = median(salary)
  )
```

4. Importing NBA data, renaming columns, and removing NAs
```
NBA_1984_2018_url <- getURL('https://raw.githubusercontent.com/sashabotsul/data332_final/refs/heads/main/data/NBA_Salaries_1985to2018.csv')
df_NBA_1984_2018 <- read.csv(text = NBA_1984_2018_url)
df_NBA_1984_2018 <- df_NBA_1984_2018 %>% select(-c(season, season_end, player_id, league))
df_NBA_1984_2018 <- df_NBA_1984_2018 %>% rename(Year = season_start)
df_NBA_1984_2018 <- df_NBA_1984_2018 %>% rename(teamID = team)

df_NBA_1984_2018 <- df_NBA_1984_2018 %>%
  mutate(teamID = na_if(teamID, "")) %>%
  drop_na() %>%
  filter(Year >= 1985)
```

5. Fixing NBA team names and applying to dataframe
```
#fix team names
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

# Apply mapping to dataset
df_NBA_1984_2018 <- df_NBA_1984_2018 %>%
  mutate(teamID = ifelse(teamID %in% names(team_replacement_map), team_replacement_map[teamID], teamID))
```

5. Creating pivot table with MLB data, to group by year and summarise salary data
```
df_nba_with_teams <- df_NBA_1984_2018 %>%
  group_by(Year, teamID) %>%
  summarise(
    num_players = n(),
    mean_salary = mean(salary),
    median_salary = median(salary)
  )
```

6. Adding sport column
```
df_mlb_with_teams <- df_mlb_with_teams %>% mutate(sport = "MLB")
df_nba_with_teams <- df_nba_with_teams %>% mutate(sport = "NBA")
```

7. Combining datasets
```
df_combined_with_teams <- bind_rows(df_mlb_with_teams, df_nba_with_teams)
```

8. Importing Consumer Price Index (CPI) data
```
df_CPI <- read_excel('data/US_CPI_DATA.xlsx', .name_repair = 'universal')
```

9. Average out CPI data and calculate the inflation rate
```
df_CPI <- df_CPI %>%
  select(-c(HALF1, HALF2)) %>%
  mutate(Annual_Avg = rowMeans(select(., -Year), na.rm = TRUE)) %>%
  mutate(Inflation_Rate = (Annual_Avg - lag(Annual_Avg)) / lag(Annual_Avg) * 100) %>%
  select(-c(Jan, Feb, Mar, Apr, May, Jun, Jul, Aug, Sep, Oct, Nov, Dec)) %>%
  filter(Year >= 1985)
```

10. Combine salary data with inflation data
```
df_combined_with_teams <- left_join(df_combined_with_teams, df_CPI, by = "Year")
```

11. Read in MLB Win data and reformat it to cooperate with the main dataset
```
mlb_wins_url <- getURL('https://raw.githubusercontent.com/sashabotsul/data332_final/refs/heads/main/data/mlb_wins.csv')
df_mlb_wins <- read.csv(text = mlb_wins_url)

#filter out not needed years and data for old teams
df_mlb_wins <- df_mlb_wins %>%
  filter(Year >= 1985) %>%
  filter(Year < 2025) %>%
  select(where(~ !all(is.na(.))))

#reformat dataset to cooperate
df_mlb_wins <- df_mlb_wins %>%
  pivot_longer(cols = -c(Year, G),  # Select all columns except Year and G
               names_to = "teamID",    # New column to store team names
               values_to = "wins") %>%
  mutate(win_loss = wins / G) %>%  # Calculate win-loss ratio
  select(-wins, -G) %>%  # Drop original wins and games played columns
  mutate(teamID = ifelse(teamID %in% names(replacement_map_mlb), replacement_map_mlb[teamID], teamID))
```

12. Read in both NBA win datasets and clean themm to pre-established format
```
#read in NBA wins data to 2017
nba_wins_to2018_url <- getURL('https://raw.githubusercontent.com/sashabotsul/data332_final/refs/heads/main/data/nba_Team_Records.csv')
df_nba_wins_to2018 <- read.csv(text = nba_wins_to2018_url)

#clean data
df_nba_wins_to2018 <- df_nba_wins_to2018 %>%
  select(Team, Season, W.L.) %>%
  mutate(Season = as.integer(sub("-.*", "", Season)) + 1) %>%
  mutate(Team = gsub("\\*", "", Team)) %>%
  rename(teamID = Team) %>%
  rename(win_loss = W.L.) %>%
  rename(Year = Season) %>%
  filter(Year >= 1985) %>%
  mutate(teamID = ifelse(teamID %in% names(team_replacement_map), team_replacement_map[teamID], teamID))

#read in remaining years need
nba_wins_to2023_url <- getURL('https://raw.githubusercontent.com/sashabotsul/data332_final/refs/heads/main/data/nba_team_stats_00_to_23.csv')
df_nba_wins_to2023 <- read.csv(text = nba_wins_to2023_url)

#clean data
df_nba_wins_to2023 <- df_nba_wins_to2023 %>%
  select(Team, win_percentage, season) %>%
  mutate(season = as.integer(sub("-.*", "", season)) + 1) %>%
  rename(Year = season) %>%
  rename(teamID = Team) %>%
  rename(win_loss = win_percentage) %>%
  filter(Year >= 2019) %>%
  filter(Year < 2024) %>%
  mutate(teamID = ifelse(teamID %in% names(team_replacement_map), team_replacement_map[teamID], teamID))
```

13. Bind win data together
```
#join win data together
df_wins <- bind_rows(
  df_mlb_wins, df_nba_wins_to2018, df_nba_wins_to2023
)
```

14. Join win data into combined dataset
```
#join into combinded dataset
df_combined_with_teams <- left_join(df_combined_with_teams, df_wins, by = c("Year", "teamID"))
write.csv(df_combined_with_teams, "combined_data_with_teams.csv")
```

## Creating our charts 📊
1. Salary Trend Plot Line
- Chart
```
output$salaryTrendPlot <- renderPlot({
    df <- filtered_data()
    if (nrow(df) == 0) return(NULL)
    df %>%
      group_by(Year) %>%
      summarise(mean_salary = mean(mean_salary, na.rm = TRUE)) %>%
      ggplot(aes(x = Year, y = mean_salary)) +
      geom_line(color = "steelblue", linewidth = 1.2) +
      labs(title = paste("Average Salary Over Time -", input$selected_league),
           x = "Year", y = "Average Salary") +
      scale_y_continuous(labels = label_comma()) +
      theme_minimal()
  })
```

2. Salary growth vs Inflation
- Chart
```
 output$salary_growth_plot <- renderPlot({
    df <- filtered_data_inflation()
    req(df)
    salary_summary <- df %>%
      group_by(sport, Year) %>%
      summarise(avg_salary = mean(mean_salary, na.rm = TRUE), inflation = mean(Inflation_Rate, na.rm = TRUE), .groups = 'drop') %>%
      arrange(sport, Year) %>%
      group_by(sport) %>%
      mutate(salary_growth = (avg_salary - lag(avg_salary)) / lag(avg_salary), inflation_pct = inflation / 100) %>%
      filter(!is.na(salary_growth)) %>%
      ungroup()
    
    plot_data <- salary_summary %>%
      pivot_longer(cols = c("salary_growth", "inflation_pct"), names_to = "metric", values_to = "value") %>%
      mutate(metric = recode(metric, "salary_growth" = "Salary Growth", "inflation_pct" = "Inflation"))
    
    ggplot(plot_data, aes(x = Year, y = value, color = sport, linetype = metric)) +
      geom_line(size = 1.2) +
      scale_y_continuous(labels = percent) +
      scale_linetype_manual(values = c("Salary Growth" = "solid", "Inflation" = "dotted")) +
      labs(title = "Salary Growth vs Inflation by League", x = "Year", y = "Percentage (%)", color = "League", linetype = "Metric") +
      theme_minimal()
  })
```

3. Salary Heat Map
- Chart
```
output$salaryHeatmap <- renderPlot({
    df <- filtered_data()
    if (nrow(df) == 0) return(NULL)
    ggplot(df, aes(x = teamID, y = factor(Year), fill = mean_salary)) +
      geom_tile(color = "white") +
      scale_fill_gradient(low = "lightblue", high = "darkblue", name = "Avg Salary", labels = label_comma()) +
      labs(title = paste("Average Salary Heatmap -", input$selected_league),
           x = "Team", y = "Year") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  })  
```

4. Salary by Year Plotted by Team
- Chart
```
output$salary_by_year_plot <- renderPlot({
    req(input$salary_metric, input$selected_league)
    
    df <- filtered_data() 
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
```


