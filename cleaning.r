library(shiny)
library(dplyr)
library(tidyr)
library(RCurl)
library(readxl)

rm(list = ls())
setwd('C:/Users/retai/Documents/r_projects/sports_salaries')

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

df_mlb_1985_2024 <- bind_rows(
  df_mlb_1985_2012,
  df_mlb_2011_2024
)

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

df_mlb_with_teams <- df_mlb_1985_2024 %>%
  group_by(Year, teamID) %>%
  summarise(
    num_players = n(),
    mean_salary = mean(salary),
    median_salary = median(salary)
  )

df_mlb <- df_mlb_1985_2024 %>%
  group_by(Year) %>%
  summarise(
    num_players = n(),
    mean_salary = mean(salary),
    median_salary = median(salary)
  )

df_mlb_teams <- df_mlb_1985_2024 %>%
  group_by(teamID) %>%
  summarise(
    count = n()
  )

#read in mlb teams
df_mlb_wins <- read_excel('data/mlb_wins.xlsx', .name_repair = 'universal')
df_mlb_wins <- df_mlb_wins %>%
  filter(Year >= 1985) %>%
  select(where(~ !all(is.na(.))))



NBA_1984_2018_url <- getURL('https://raw.githubusercontent.com/sashabotsul/data332_final/refs/heads/main/data/NBA_Salaries_1985to2018.csv')
df_NBA_1984_2018 <- read.csv(text = NBA_1984_2018_url)
df_NBA_1984_2018 <- df_NBA_1984_2018 %>% select(-c(season, season_end, player_id, league))
df_NBA_1984_2018 <- df_NBA_1984_2018 %>% rename(Year = season_start)
df_NBA_1984_2018 <- df_NBA_1984_2018 %>% rename(teamID = team)

df_NBA_1984_2018 <- df_NBA_1984_2018 %>%
  mutate(teamID = na_if(teamID, "")) %>%
  drop_na() %>%
  filter(Year >= 1985)

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


df_nba_with_teams <- df_NBA_1984_2018 %>%
  group_by(Year, teamID) %>%
  summarise(
    num_players = n(),
    mean_salary = mean(salary),
    median_salary = median(salary)
  )

df_nba <- df_NBA_1984_2018 %>%
  group_by(Year) %>%
  summarise(
    num_players = n(),
    mean_salary = mean(salary),
    median_salary = median(salary)
  )

# Add a source column to each dataset
df_mlb_with_teams <- df_mlb_with_teams %>% mutate(sport = "MLB")
df_nba_with_teams <- df_nba_with_teams %>% mutate(sport = "NBA")

# Combine both datasets
df_combined_with_teams <- bind_rows(df_mlb_with_teams, df_nba_with_teams)

#read in CPI Data
df_CPI <- read_excel('data/US_CPI_DATA.xlsx', .name_repair = 'universal')

#average out CPI values per year and calculate the inflation rate
df_CPI <- df_CPI %>%
  select(-c(HALF1, HALF2)) %>%
  mutate(Annual_Avg = rowMeans(select(., -Year), na.rm = TRUE)) %>%
  mutate(Inflation_Rate = (Annual_Avg - lag(Annual_Avg)) / lag(Annual_Avg) * 100) %>%
  select(-c(Jan, Feb, Mar, Apr, May, Jun, Jul, Aug, Sep, Oct, Nov, Dec)) %>%
  filter(Year >= 1985)
  
#join in inflation data
df_combined_with_teams <- left_join(df_combined_with_teams, df_CPI, by = "Year")
write.csv(df_combined_with_teams, "combined_data_with_teams.csv")

