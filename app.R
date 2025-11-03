library(tidyverse)
library(shiny)
library(bslib)


# read in the NBA play by play data from the 2020-21 season
nba_pbp <- read_csv("NBA_PBP_2020-21.csv")

#create a final score variable for the away and home teams
nba_pbp <- nba_pbp |> 
  group_by(URL) |> 
  mutate(away_final_score = last(AwayScore),
         home_final_score = last(HomeScore),
         total_points = away_final_score + home_final_score) |> 
  ungroup()

# create a conference variable for home and away teams in the play by play data
nba_pbp <- nba_pbp |> 
  mutate(away_conference = ifelse(AwayTeam %in% c("BOS", "PHI", "NYK", "TOR", "BKN", "MIA", "CHO", "ORL", "WAS", "ATL", "CHI", "CLE", "MIL", "DET", "IND"), "Eastern", "Western"),
         home_conference = ifelse(HomeTeam %in% c("BOS", "PHI", "NYK", "TOR", "BKN", "MIA", "CHO", "ORL", "WAS", "ATL", "CHI", "CLE", "MIL", "DET", "IND"), "Eastern", "Western"))

# user interface for the shiny app
ui <- fluidPage(
  h1("ST 558 Project 2: NBA Play-by-Play Data"),
  sidebarLayout(
    sidebarPanel(
      h2("Subset the Data"),
      # radio button for away team's conference
      radioButtons(inputId = "away_conf",
                   label = "Away Team's Conference:",
                   choices = c(
                     "All" = "all",
                     "Eastern" = "eastern",
                     "Western" = "western"
                   ),
                   selected = "all"),
      
      # radio button for home team's conference
      radioButtons(inputId = "home_conf",
                   label = "Home Team's Conference:",
                   choices = c(
                     "All" = "all",
                     "Eastern" = "eastern",
                     "Western" = "western"
                   ),
                   selected = "all"),
      # select first numeric variable that can be subsetted with a slider
      selectizeInput(inputId = "first_num_select",
                     label = "Numeric Variable to Subset:",
                     choices = c(
                       "Away Score" = "away_final_score",
                       "Home Score" = "home_final_score",
                       "Total Combined Points" = "total_points"
                     ),
                     selected = "away_final_score"),
      
      # slider for the first numeric variable to be subsetted on
      sliderInput(inputId = "first_num_slider",
                  label = "Numeric Values:",
                  min = 70, max = 150, value = c(70, 100)),
      # select second numeric variable that can be subsetted with a slider
      selectizeInput(inputId = "second_num_select",
                     label = "Numeric Variable to Subset:",
                     choices = c(
                       "Away Score" = "away_final_score",
                       "Home Score" = "home_final_score",
                       "Total Combined Points" = "total_points"
                     ),
                     selected = "home_final_score"), 
      # slider for the first numeric variable to be subsetted on
      sliderInput(inputId = "second_num_slider",
                  label = "Numeric Values:",
                  min = 70, max = 150, value = c(70, 150))
    ),
      
    # main panel of the user interface
    mainPanel(
      tabsetPanel(
        tabPanel("About", "contents! As usual, separate items by commas and you can put widgets, outputs, and HTML content"),
        tabPanel("Data Download", "contents"),
        tabPanel("Data Exploration", "contents")
      )
    )
  )
)
# server function for the shiny app
server <- function(input, output, session) {
  
}

# run the application
shinyApp(ui = ui, server = server)