library(plyr);library(dplyr);library(stringr);library(rvest);library(lubridate);library(readr);library(tidyr);library(xts);library(dygraphs);library(lazyeval);library(RColorBrewer);library(shiny);library(shinythemes)
ui <- fluidPage(
     theme = shinytheme("united"),
     tags$h2(style = "font-family:Impact",
             "Daily Fantasy Baseball Player Lineup Optimization"),
     tags$p(style = "font-family:Calibri",
            "Group 1: Disha An, Luis Flores, 
            Mike Nestel, Nick Benvenuto"),
     tags$hr(), # Draw a line
     
     #CREATE A SIDEBAR PANEL TO UPLOAD DFS LINK
     sidebarPanel(
          selectInput("Dominant Platforms", "Please choose a platform:",
                      c('DraftKings', 'FanDuel') ),
          textOutput("PlatformChosen"),
          tags$hr(),
          
          #Link to be provided by user from Draft Kings
          textInput("dk_file", "link to DFS csv file")
     ),
     
     
     mainPanel(
          h3("All of the baseball data is in your hands!"),
          p("Powered by FE550 Team 1"),
          
          tabsetPanel(
               tabPanel("Optimal Lineup Tuning", tags$hr(), tags$p(style = "font-family:Calibri", "Today's optimized lineup without the opposition team you don't want: "), 
                        textInput("oppo_not", "Opposition team names you don't want to include: ", "BOS"), 
                        selectInput("risk_tol", "Choose your risk tolerance", c("Low Risk", "Medium Risk", "High Risk")),
                        sliderInput("min_pa", "Choose minimum plate appearences for each batter", min = 0, max = 100, value = 50),
                        checkboxInput("use_pitch_bat", "Use Pitcher vs. Batter in Projections?", value = FALSE),
                        tableOutput("Optimized2"), tags$hr()),
               
               tabPanel("Player Comparison Tool", textInput("players_name", "Players you want to compare: ", "Bryce Harper, Mike Trout"), 
                        textOutput("right_name"), 
                        selectInput("baseball_stat", "CHOOSE A BASEBALL STATISTIC: ", c("PA", "AB", "R", "H", "2B", "3B", "HR", "RBI", "BB", "IBB", "SO", "HBP", "SH", "SF", "ROE", "GDP", "SB", "CS", "BOP", "DFS.DK.", "DFS.FD.", "BA", "OBP", "SLG", "OPS")), 
                        selectInput("compare_type", "Type of Comparison", c("season", "weekly", "daily")),
                        mainPanel(
                             tabsetPanel(
                                  id = 'Visualizations',
                                  tabPanel('Comparison Among Players', dygraphOutput("Compare1"))))
               )
          )
     )
     )