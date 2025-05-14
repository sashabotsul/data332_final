library(shiny)
library(dplyr)
library(tidyr)
library(RCurl)
library(readxl)
library(readr)

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
df_NBA_1984_2018 <- df_NBA_1984_2018 %>% select(-c(season, season_start, player_id, league))
df_NBA_1984_2018 <- df_NBA_1984_2018 %>% rename(Year = season_end)
df_NBA_1984_2018 <- df_NBA_1984_2018 %>% rename(teamID = team)

df_NBA_1984_2018 <- df_NBA_1984_2018 %>%
  mutate(teamID = na_if(teamID, "")) %>%
  drop_na() %>%
  filter(Year >= 1985)

#read in more salary data, dataset does not have teams however
NBA_1990_2023_url <- getURL('https://raw.githubusercontent.com/sashabotsul/data332_final/refs/heads/main/data/NBA_Salaries1990-2023.csv')
df_NBA_1990_2023 <- read.csv(text = NBA_1990_2023_url)

df_NBA_1990_2023 <- df_NBA_1990_2023 %>%
  rename(Year = seasonStartYear, Player = playerName) %>%  # Rename columns
  mutate(Year = Year + 1,  # Add 1 to every value in Year
         salary = parse_number(salary)) %>%  # Convert salary from character with commas to numeric
  select(-inflationAdjSalary, -X) %>%
  filter(Year >= 2018)

#dataset has teams and the same names as the above but not salary
NBA_1990_2023_playernames <- getURL('https://raw.githubusercontent.com/sashabotsul/data332_final/refs/heads/main/data/NBA_Player_Stats_1950-2022.csv')
df_NBA_playernames <- read.csv(text = NBA_1990_2023_playernames)

#clean not need data to basically just turn this into a team name key
df_NBA_playernames <- df_NBA_playernames %>%
  select(Player, Season, Tm) %>%  # Keep only Player and Season columns
  rename(Year = Season) %>%   # Rename Season to Year
  filter(Year >= 2018)        # Remove rows where Year is before 2018

#join the salary and teams together for the extension
df_NBA_2018_2022 <- left_join(df_NBA_playernames, df_NBA_1990_2023, by = c("Player", "Year"))

# Create a lookup table for team abbreviations
team_lookup <- tibble(
  Tm = c("ATL", "BOS", "BRK", "CHO", "CHI", "CLE", "DAL", "DEN", "DET",
         "GSW", "HOU", "IND", "LAC", "LAL", "MEM", "MIA", "MIL", "MIN",
         "NOP", "NYK", "OKC", "ORL", "PHI", "PHO", "POR", "SAC", "SAS",
         "TOR", "UTA", "WAS"),
  teamName = c("Atlanta Hawks", "Boston Celtics", "Brooklyn Nets",
               "Charlotte Hornets", "Chicago Bulls", "Cleveland Cavaliers",
               "Dallas Mavericks", "Denver Nuggets", "Detroit Pistons",
               "Golden State Warriors", "Houston Rockets", "Indiana Pacers",
               "Los Angeles Clippers", "Los Angeles Lakers", "Memphis Grizzlies",
               "Miami Heat", "Milwaukee Bucks", "Minnesota Timberwolves",
               "New Orleans Pelicans", "New York Knicks", "Oklahoma City Thunder",
               "Orlando Magic", "Philadelphia 76ers", "Phoenix Suns",
               "Portland Trail Blazers", "Sacramento Kings", "San Antonio Spurs",
               "Toronto Raptors", "Utah Jazz", "Washington Wizards")
)

# Transform the dataset
df_NBA_2018_2022 <- df_NBA_2018_2022 %>%
  left_join(team_lookup, by = "Tm") %>%  # Replace abbreviation with full team name
  rename(teamID = teamName) %>%  # Rename the new column
  select(-Tm, -Player) %>%  # Drop original team code and Player column
  drop_na() %>%
  mutate(Year = as.integer(Year),
         salary = as.integer(salary))

#join 2023 data
NBA_2023_url <- getURL('https://raw.githubusercontent.com/sashabotsul/data332_final/refs/heads/main/data/nba_salaries_2023.csv')
df_NBA_2023 <- read.csv(text = NBA_2023_url)

team_lookup2 <- tibble(
  Team = c("ATL", "BOS", "BRK", "CHO", "CHI", "CLE", "DAL", "DEN", "DET",
         "GSW", "HOU", "IND", "LAC", "LAL", "MEM", "MIA", "MIL", "MIN",
         "NOP", "NYK", "OKC", "ORL", "PHI", "PHO", "POR", "SAC", "SAS",
         "TOR", "UTA", "WAS"),
  teamName = c("Atlanta Hawks", "Boston Celtics", "Brooklyn Nets",
               "Charlotte Hornets", "Chicago Bulls", "Cleveland Cavaliers",
               "Dallas Mavericks", "Denver Nuggets", "Detroit Pistons",
               "Golden State Warriors", "Houston Rockets", "Indiana Pacers",
               "Los Angeles Clippers", "Los Angeles Lakers", "Memphis Grizzlies",
               "Miami Heat", "Milwaukee Bucks", "Minnesota Timberwolves",
               "New Orleans Pelicans", "New York Knicks", "Oklahoma City Thunder",
               "Orlando Magic", "Philadelphia 76ers", "Phoenix Suns",
               "Portland Trail Blazers", "Sacramento Kings", "San Antonio Spurs",
               "Toronto Raptors", "Utah Jazz", "Washington Wizards")
)

#clean dataset
df_NBA_2023 <- df_NBA_2023 %>%
  mutate(Team = sub("/.*", "", Team)) %>%
  mutate(year = 2023) %>%
  left_join(team_lookup2, by = "Team") %>%
  rename(salary = Salary) %>%
  rename(teamID = teamName) %>%
  select(year, salary, teamID)

df_nba_1985_2022 <- bind_rows(
  df_NBA_1984_2018,
  df_NBA_2018_2022,
  df_NBA_2023
)

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
df_nba_1985_2022 <- df_nba_1985_2022 %>%
  mutate(teamID = ifelse(teamID %in% names(team_replacement_map), team_replacement_map[teamID], teamID))

df_nba_with_teams <- df_nba_1985_2022 %>%
  group_by(Year, teamID) %>%
  summarise(
    num_players = n(),
    mean_salary = mean(salary),
    median_salary = median(salary)
  )

df_nba <- df_nba_1985_2022 %>%
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

