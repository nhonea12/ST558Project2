# work on creating tables and visualizations statically in the document
library(tidyverse)
library(janitor)
#install.packages("see")
library(see)
install.packages("ggridges")
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

# 
ggplot(data = nba_pbp, aes(x = total_points, colour = home_conference)) + 
  geom_density()

ggplot(data = nba_pbp, aes(x = away_final_score, colour = away_conference)) + 
  geom_histogram()

ggplot(data = nba_pbp, aes(x = home_final_score, colour = home_conference)) + 
  geom_histogram()


nba_pbp |> 
  group_by(URL) |> 
  summarise(shot_dist_mean)


ggplot(data = nba_pbp, aes(x = home_final_score))

# use of plots with something not covered in class (ggridges):
# plot of each away team's points scored, colored by conference, done with ggridges package
ggplot(data = nba_pbp, aes(x = away_final_score, y = AwayTeam, fill = away_conference)) + 
  geom_density_ridges() + 
  labs(
    title = "Away team's points scored", 
    subtitle = "From play-by-play data of part of 2020-21 NBA season",
    x = "Away Team Points Scored",
    y = "Away Team"
  ) + 
  scale_fill_discrete(name = "Conference")

# plot of each home team's points scored, colored by conference, done with ggridges package
ggplot(data = nba_pbp, aes(x = home_final_score, y = HomeTeam, fill = home_conference)) + 
  geom_density_ridges() + 
  labs(
    title = "Home team's points scored", 
    subtitle = "From play-by-play data of part of 2020-21 NBA season",
    x = "Home Team Points Scored",
    y = "Home Team"
      ) + 
  scale_fill_discrete(name = "Conference")


