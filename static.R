# work on creating tables and visualizations statically in the document
library(tidyverse)
library(janitor)

nba_pbp <- read_csv("NBA_PBP_2020-21.csv")
view(nba_pbp)
unique(nba_pbp$ShotType)
unique(nba_pbp$URL)
unique(nba_pbp$AwayTeam)

# one-way contingency table of shot types
table("Shot type" = nba_pbp$ShotType)

# two-way contingency table of shot types and results
table("Shot type" = nba_pbp$ShotType,
      "Shot Result" = nba_pbp$ShotOutcome)

# one-way contingency table of shot types using tabyl() from janitor package
nba_pbp |> 
  tabyl(ShotType, show_na = FALSE)

# one-way contingency table of shot types using tabyl() from janitor package
nba_pbp |> 
  tabyl(ShotType, ShotOutcome, show_na = FALSE)

# find the mean distance of made shots and missed shots
nba_pbp |> 
  filter(!is.na(ShotDist)) |> 
  group_by(ShotOutcome) |> 
  summarise(meanDist = mean(ShotDist))

# find the mean and standard deviation of final scores for away and home teams
nba_pbp |> 
  group_by(URL) |> 
  mutate(away_final_score = last(AwayScore),
         home_final_score = last(HomeScore)) |> 
  ungroup() |> 
  summarise(away_score_mean = mean(away_final_score),
            home_score_mean = mean(home_final_score),
            away_score_sd = sd(away_final_score),
            home_score_sd = sd(home_final_score)
            )

# create a conference variable for home and away teams in the play by play data
nba_pbp <- nba_pbp |> 
  mutate(away_conference = ifelse(AwayTeam %in% c("BOS", "PHI", "NYK", "TOR", "BKN", "MIA", "CHO", "ORL", "WAS", "ATL", "CHI", "CLE", "MIL", "DET", "IND"), "Eastern", "Western"),
         home_conference = ifelse(HomeTeam %in% c("BOS", "PHI", "NYK", "TOR", "BKN", "MIA", "CHO", "ORL", "WAS", "ATL", "CHI", "CLE", "MIL", "DET", "IND"), "Eastern", "Western"))
