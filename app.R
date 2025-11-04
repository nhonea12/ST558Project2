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
      selectInput(inputId = "first_num_select",
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
                  min = 70, max = 150, value = c(70, 150)),
      
      # allow the user to set the minimum for the slider with dynamic UI
      fluidRow(
        column(7,
               numericInput(inputId = "first_min_value",
                            label = "Set slider min:",
                            min = 50,
                            max = 299,
                            value = 70)),
        column(5,
               actionButton(inputId = "first_min_button",
                            label = "Update the slider"
               ))
      ),

      # allow the user to set the maximum for the slider with dynamic UI
      fluidRow(
        column(7,
               numericInput(inputId = "first_max_value",
                            label = "Set slider min:",
                            min = 51,
                            max = 300,
                            value = 150)),
        column(5,
               actionButton(inputId = "first_max_button",
                            label = "Update the slider"
               ))
      ),
      
      # select second numeric variable that can be subsetted with a slider
      selectInput(inputId = "second_num_select",
                     label = "Numeric Variable to Subset:",
                     choices = c(
                       "Away Score" = "away_final_score",
                       "Home Score" = "home_final_score",
                       "Total Combined Points" = "total_points"
                     ),
                     selected = "home_final_score"), 
      
      # slider for the second numeric variable to be subsetted on
      sliderInput(inputId = "second_num_slider",
                  label = "Numeric Values:",
                  min = 70, max = 150, value = c(70, 150)),
    
    # allow the user to set the minimum for the slider with dynamic UI
      fluidRow(
        column(6,
               numericInput(inputId = "second_min_value",
                            label = "Set slider min:",
                            min = 50,
                            max = 299,
                            value = 70)),
        column(6,
               actionButton(inputId = "second_min_button",
                            label = "Update the slider"
               ))
      ),
      
      # allow the user to set the maximum for the slider with dynamic UI
      fluidRow(
         column(6,
                numericInput(inputId = "second_max_value",
                             label = "Set slider min:",
                             min = 51,
                             max = 300,
                             value = 150)),
         column(6,
                actionButton(inputId = "second_max_button",
                             label = "Update the slider"
                ))
        ),
    
      h3("Press Here To Subset!"),
      # action button to subset the data
      actionButton("subset_button","Subset the Data")
    ),
      
    # main panel of the user interface
    mainPanel(
      tabsetPanel(
        tabPanel("About", 
                  markdown(
          glue::glue("This app includes data from 209 regular season games played during the 2020-21 NBA season. You are able to view the points scored by by both the home and away teams in each game within the data, and can view how they interact with other variables, like the conferences of the teams. \n 
                  The data can be found on [kaggle](https://www.kaggle.com/datasets/schmadam97/nba-playbyplay-data-20182019). \n
                     The sidebar allows you to subset the data you are viewing. So if you only want to look at games where the home or away team was in a specific conference (Eastern or Western), you can do that! You can also subset by the amount of points scored by the home or away team, or the total combined points scored in a game, and only get the play-by-play data from those specified games. \n
                     The other tabs in this app allow you to view/download and explore the data! The 'Data Download' tab allows you to view the play-by-play data, only showing a subset of it if you specified a subset on the sidebar panel. It also includes a button that you can click to download the data! \n 
                     The 'Data Exploration' tab includes numeric and graphical summaries of the data. This includes summaries like the mean distances of different shot types, and the mean distance of made and missed shots. Also contingency tables of shot types and their results (made or missed). There are also graphical summaries of points scored by the home and away team (grouped by conference), total points scored in a game (grouped by the conference of the home team), scatterplots of the scores of each team in every game, faceted by the conference of the home and away teams, and ridge plots, showing the density of teams' points scored and allowed, both as the home team and the away team.")
            ),
          tags$img(
            src = "nba-logo.png",
            width = "500px"
            )
          ),
        tabPanel("Data Download", p("Data download content coming soon")),
        tabPanel("Data Exploration", p("Data exploration content coming soon"))
      )
    )
  )
)
# server function for the shiny app
server <- function(input, output, session) {

  # update the first slider minimum based on the number given by the user
  observeEvent(input$first_min_button,
               updateSliderInput(session,
                                 "first_num_slider",
                                 min = input$first_min_value))

  # update the first slider maximum based on the number given by the user
  observeEvent(input$first_max_button,
               updateSliderInput(session,
                                 "first_num_slider",
                                 max = input$first_max_value))

  # update the second slider minimum based on the number given by the user
  observeEvent(input$second_min_button,
               updateSliderInput(session,
                                 "second_num_slider",
                                 min = input$second_min_value))

  # update the second slider maximum based on the number given by the user
  observeEvent(input$second_max_button,
               updateSliderInput(session,
                                 "second_num_slider",
                                 max = input$second_max_value))
}
# run the application
shinyApp(ui = ui, server = server)