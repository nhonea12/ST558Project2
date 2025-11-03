# work on creating tables and visualizations statically in the document
library(tidyverse)
library(janitor)
#install.packages("see")
library(see)
#install.packages("ggridges")
library(ggridges)


nba_pbp <- read_csv("NBA_PBP_2020-21.csv")
view(nba_pbp)
unique(nba_pbp$ShotType)
unique(nba_pbp$URL)
unique(nba_pbp$AwayTeam)
unique(nba_pbp$Quarter)

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

# find the mean shot distance of different shot types
nba_pbp |> 
  filter(!is.na(ShotDist)) |> 
  group_by(ShotType) |> 
  summarise(shot_dist_mean = mean(ShotDist, na.rm = TRUE)) |> 
  ungroup()

#create a final score variable for the away and home teams
nba_pbp <- nba_pbp |> 
  group_by(URL) |> 
  mutate(away_final_score = last(AwayScore),
         home_final_score = last(HomeScore),
         total_points = away_final_score + home_final_score) |> 
  ungroup()

# create a score by quarter variable for the away and home teams
nba_pbp <- nba_pbp |> 
  group_by(URL, Quarter) |> 
  mutate(away_quarter_score = last(AwayScore),
         home_quarter_score = last(HomeScore),
         total_quarter_points = away_quarter_score + home_quarter_score) |> 
  ungroup()

# find the mean and standard deviation of final scores for away and home teams
nba_pbp |> 
  summarise(away_score_mean = mean(away_final_score),
            home_score_mean = mean(home_final_score),
            away_score_sd = sd(away_final_score),
            home_score_sd = sd(home_final_score)
            )

# create a conference variable for home and away teams in the play by play data
nba_pbp <- nba_pbp |> 
  mutate(away_conference = ifelse(AwayTeam %in% c("BOS", "PHI", "NYK", "TOR", "BKN", "MIA", "CHO", "ORL", "WAS", "ATL", "CHI", "CLE", "MIL", "DET", "IND"), "Eastern", "Western"),
         home_conference = ifelse(HomeTeam %in% c("BOS", "PHI", "NYK", "TOR", "BKN", "MIA", "CHO", "ORL", "WAS", "ATL", "CHI", "CLE", "MIL", "DET", "IND"), "Eastern", "Western"))

# density plot of the total points scored in a game, grouped by whether the game was hosted by a Eastern or Western conference team
nba_pbp |> 
  group_by(URL, home_conference) |> 
  summarise(total_points = last(total_points)) |> 
  ungroup() |> 
ggplot(aes(x = total_points, colour = home_conference)) + 
  geom_density(adjust = 0.5) + 
  labs(
    title = "Density of Total Points Scored in NBA Games",
    subtitle = "Grouped by Conference of Home Team (Data from 2020-21 NBA Season)",
    x = "Total Combined Points in Game",
    y = "Density"
  ) + 
  scale_color_discrete(name = "Conference")

# histogram of the final score of the away team, grouped by their conference
nba_pbp |> 
  group_by(URL, away_conference) |> 
  summarise(away_final_score = last(away_final_score)) |> 
  ungroup() |> 
ggplot(aes(x = away_final_score, colour = away_conference)) + 
  geom_histogram(binwidth = 5) + 
  labs(
    title = "Final Score of Away Teams",
    subtitle = "Grouped by Conference (Data from 2020-21 NBA Season",
    x = "Away Team Final Score",
    y = "Frequency"
  ) + 
  scale_color_discrete(name = "Conference")

# histogram of the final score of the home team, grouped by their conference
nba_pbp |> 
  group_by(URL, home_conference) |> 
  summarise(home_final_score = last(home_final_score)) |> 
  ungroup() |> 
ggplot(aes(x = home_final_score, colour = home_conference)) + 
  geom_histogram(binwidth = 5) + 
  labs(
    title = "Final Score of Home Teams",
    subtitle = "Grouped by Conference (Data from 2020-21 NBA Season",
    x = "Home Team Final Score",
    y = "Frequency"
  ) + 
  scale_color_discrete(name = "Conference")

# use a plot with faceting:
# scatter plot with faceting the home and away finals scores of every game, faceted by the conferences of the home and away team
nba_pbp |> 
  group_by(URL, away_conference, home_conference) |> 
  summarise(
    away_final_score = last(away_final_score),
    home_final_score = last(home_final_score)
    ) |> 
  ungroup() |> 
ggplot(aes(x = away_final_score, y = home_final_score)) + 
  geom_jitter() + 
  facet_grid(away_conference~home_conference) + 
  labs(
    title = "Home and Away Scores of Games",
    subtitle = "Grouped by Conferences of The Teams (Rows are away conference, columns are home conference)",
    x = "Away Team Score",
    y = "Home Team Score"
  )

# use of plots with something not covered in class (ggridges):
# density plot of each away team's points scored, colored by conference, done with ggridges package
nba_pbp |> 
  group_by(URL, away_conference, AwayTeam) |> 
  summarise(away_final_score = last(away_final_score)) |> 
  ungroup() |> 
ggplot(aes(x = away_final_score, y = AwayTeam, fill = away_conference)) + 
  geom_density_ridges() + 
  labs(
    title = "Away Team's Points Scored", 
    subtitle = "From play-by-play data of part of 2020-21 NBA season",
    x = "Away Team Points Scored",
    y = "Away Team"
  ) + 
  scale_fill_discrete(name = "Conference")

# plot of each home team's points scored, colored by conference, done with ggridges package
nba_pbp |> 
  group_by(URL, home_conference, HomeTeam) |> 
  summarise(home_final_score = last(home_final_score)) |> 
  ungroup() |> 
ggplot(aes(x = home_final_score, y = HomeTeam, fill = home_conference)) + 
  geom_density_ridges() + 
  labs(
    title = "Home Team's Points Scored", 
    subtitle = "From play-by-play data of part of 2020-21 NBA season",
    x = "Home Team Points Scored",
    y = "Home Team"
      ) + 
  scale_fill_discrete(name = "Conference")

# density plot of each away team's points allowed, colored by conference, done with ggridges package
nba_pbp |> 
  group_by(URL, away_conference, AwayTeam) |> 
  summarise(away_final_allowed = last(home_final_score)) |> 
  ungroup() |> 
  ggplot(aes(x = away_final_allowed, y = AwayTeam, fill = away_conference)) + 
  geom_density_ridges() + 
  labs(
    title = "Away Team's Points Allowed", 
    subtitle = "From play-by-play data of part of 2020-21 NBA season",
    x = "Away Team Points Allowed",
    y = "Away Team"
  ) + 
  scale_fill_discrete(name = "Conference")

# plot of each home team's points allowed, colored by conference, done with ggridges package
nba_pbp |> 
  group_by(URL, home_conference, HomeTeam) |> 
  summarise(home_final_allowed = last(away_final_score)) |> 
  ungroup() |> 
  ggplot(aes(x = home_final_allowed, y = HomeTeam, fill = home_conference)) + 
  geom_density_ridges() + 
  labs(
    title = "Home Team's Points Allowed", 
    subtitle = "From play-by-play data of part of 2020-21 NBA season",
    x = "Home Team Points Scored",
    y = "Home Team"
  ) + 
  scale_fill_discrete(name = "Conference")

