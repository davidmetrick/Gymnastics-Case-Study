library(shiny)
library(tidyverse)


# Define UI for application that draws a histogram
ui <- fluidPage(
  # Application title
  titlePanel("Team Predictions"),
  
  fluidRow(
    column(6, selectInput("gender", "Gender", choices = c("Women"='w',
                                              "Men" = 'm'))),
  column(6, uiOutput("country"))),
  tabsetPanel(
    id = 'tabs',
    tabPanel(title = 'Generate Teams',
             # Sidebar with a slider input for number of bins 
             sidebarLayout(
               sidebarPanel(
                 
                 fluidRow(
                   column(width = 6, h4("Medal Weights"),
                          radioButtons("medal_weight", label = '',
                                       choices = c("Maximize total medals",
                                                   "Maximize gold medals",
                                                   "Prefer gold medals"))),
                   column(width = 6, h4("Event Weights"),
                          sliderInput("team", "Team Event", min = 0, max = 2, 
                                      value = 1, step = 1, ticks = F),
                          sliderInput("aa", "Individual All-Around", min = 0, 
                                      max = 2, value = 1, step = 1, ticks = F),
                          sliderInput("event", "Individual Apparatus", min = 0, 
                                      max = 2, value = 1, step = 1, ticks = F),
                          uiOutput("errorMessage"))
                 ),
                 # Thought: Incorporate Submit button
                 width = 6),
               
               # Show a plot of the generated distribution
               mainPanel(
                 h4("Best Team", style = 'text-align:center'),
                 tableOutput("team_table"),
                 uiOutput("exp_medals"),
                 width = 6
               )
             )),
    tabPanel(title = 'Compare Teams',
             fluidRow(
               
               column(6, 
                      h4("Team A", style = 'text-align:center;'),
                      
                      fluidRow(
                        column(2),
                        column(10, 
                               selectInput('personA1', label = '',choices = NULL),
                               div(style = "margin-top:-20px"),
                               selectInput('personA2', label = '',choices = NULL),
                               div(style = "margin-top:-20px"),
                               selectInput('personA3', label = '',choices = NULL),
                               div(style = "margin-top:-20px"),
                               selectInput('personA4', label = '',choices = NULL),
                               div(style = "margin-top:-20px"),
                               selectInput('personA5', label = '',choices = NULL))),
                      uiOutput('exp_medalsA')
               ),
               column(6, 
                      h4("Team B", style = 'text-align:center;'),
                      
                      fluidRow(
                        column(2),
                        column(10, 
                               selectInput('personB1', label = '',choices = NULL),
                               div(style = "margin-top:-20px"),
                               selectInput('personB2', label = '',choices = NULL),
                               div(style = "margin-top:-20px"),
                               selectInput('personB3', label = '',choices = NULL),
                               div(style = "margin-top:-20px"),
                               selectInput('personB4', label = '',choices = NULL),
                               div(style = "margin-top:-20px"),
                               selectInput('personB5', label = '',choices = NULL))),
                      uiOutput('exp_medalsB')
               ),
               
             )
    ) 
  )
)


# Define server logic required to draw a histogram
server <- function(input, output) {
  # source('../get.data.r')
  # source('../prep.data.r')
  
  # Put in reactive?? 
  # Read in csvs based on inputs (will be coded)
  countries_men <- c('USA', 'CHN', 'JPN', 'GBR', 'ITA', 'ESP',
                     'BRA', 'KOR', 'GER', 'CAN', 'TUR', 'HUN')
  
  countries_women <- c('USA', 'GBR', 'CAN', 'BRA', 'ITA', 'CHN',
                       'JPN', 'FRA', 'NED', 'HUN', 'ROU', 'BEL')
  
  picked_team <- reactive({
    # convert weight to number
    priority_weight <- switch(input$medal_weight,
                         "Maximize total medals" = c(1,1,1),
                         "Maximize gold medals" = c(1,0,0),
                         "Prefer gold medals" = c(3,2,1))
    weight_list <- c(input$team,input$aa,input$event)
    
    weight_list <- switch(paste0(weight_list,collapse = ""),
                     "222" = c(1,1,1),
                     "200" = c(1,0,0),
                     "020" = c(0,1,0),
                     "002" = c(0,0,1),
                     "220" = c(1,1,0),
                     "202" = c(1,0,1),
                     "022" = c(0,1,1),
                     weight_list)
    weight <- as.vector(outer(priority_weight, weight_list,"*"))
    if (file.exists(paste0('../totsims/scores-' ,
                           paste(weight,collapse="."),"-",input$country,"-",
                           input$gender,".csv"))){
      scores <- read_csv(paste0('../totsims/scores-' ,
                                paste(weight,collapse="."),"-",input$country,"-",
                                input$gender,".csv"), show_col_types = FALSE)
      
      # Do we need new names tables for everything? Probably not.
      names <- read_csv(paste0('../totsims/names-',
                               paste(weight,collapse="."),"-",input$country,"-",
                               input$gender,".csv"), show_col_types = FALSE)
      
      # Total expected medals
      scores$composite <- rowSums(scores) 
      
      best_team <- which.max(scores$composite)
      list('Athletes' = names[[best_team]], 'Exp_Medals' = scores[best_team,])
    }else{
      scoreNA = rep(NA,9)
      list('Athletes' = c(), 'Exp_Medals' = scoreNA)
    }
    
  })
  
  
  output$team_table <- renderTable({
    data.frame('Best_Team' = picked_team()[['Athletes']])}, 
    striped = T, width = '200%', colnames = F, align = 'c')
  
  # Helper for exp_medals widget
  get_medals <- function(name, group = ''){
    if (group == ''){
      return(picked_team()[['Exp_Medals']][name] |> round(digits = 2))
    } else{
      weight = as.vector(outer(c(1,1,1), c(1,1,1),"*"))
      if (file.exists(paste0('../totsims/names-',paste(weight,collapse="."),
                             "-",input$country,"-",input$gender, '.csv'))){
        names <- read_csv(paste0('../totsims/names-',paste(weight,collapse="."),
                                 "-",input$country,"-",input$gender, '.csv'), 
                          show_col_types = FALSE) |> lapply(sort) 
        scores <- read_csv(paste0('../totsims/scores-',paste(weight,collapse="."),
                                  "-",input$country,"-",input$gender, '.csv'), show_col_types = FALSE)
        teams <- list('A' = sort(c(input$personA1, input$personA2, input$personA3,
                                   input$personA4, input$personA5)),
                      'B' = sort(c(input$personB1, input$personB2, input$personB3,
                                   input$personB4, input$personB5))) 
        if (group == 'A'){
          # Get team!
          team <- scores[which(sapply(names, identical, teams[['A']])),]
          return(team[name] |> round(digits = 2))
        }
        else if (group == 'B'){
          # Get team!
          team <- scores[which(sapply(names, identical, teams[['B']])),]

          return(team[name] |> round(digits = 2))
        }
      }
    } 
  }
  
  # Helper for exp_medals widget
  medal_row <- function(long_name, short_name, group = ''){
    
    return(
      div(fluidRow(column(3, h5(long_name)),
                   column(3,  h4('🥇', style = 'text-align:center; margin:0px'),
                          h6("Gold", style = 'text-align:center; margin:3px'), 
                          div(h5(get_medals(paste0(short_name, '_g'), group)), 
                              style = 'border-style:solid; text-align:center;
                          background-color:#ffffff')),
                   column(3,  h4('🥈', style = 'text-align:center; margin:0px'), 
                          h6("Silver", style = 'text-align:center; margin:3px'), 
                          div(h5(get_medals(paste0(short_name, '_s'), group)), 
                              style = 'border-style:solid; text-align:center;
                          background-color:#ffffff')),
                   column(3,   h4('🥉', style = 'text-align:center; margin:0px'),
                          h6("Bronze", style = 'text-align:center; margin:3px'), 
                          div(h5(get_medals(paste0(short_name, '_b'), group)), 
                              style = 'border-style:solid; text-align:center;
                          background-color:#ffffff')),
                   style = 'background-color:#94cef2; border-style:solid;
               margin:10px; padding:5px;')
      ))}
  
  # Add in some CSS here to make it look nicer :) 
  output$exp_medals <- renderUI({
    div(h4("Expected Medals", style = 'text-align:center;'),
        medal_row('Team', 'team'),
        medal_row('Indiv. All Around', 'aa'),
        medal_row('Indiv. Apparatus', 'event')
    )
  })
  
  countries_men <- c('USA', 'CHN', 'JPN', 'GBR', 'ITA', 'ESP',
                   'GER', 'CAN', 'TUR', 'SUI', 'NED', 'UKR')

  countries_women <- c('USA', 'GBR', 'CAN', 'BRA', 'ITA', 'CHN',
                     'JPN', 'FRA', 'NED', 'ROU', 'AUS', 'KOR')
  
  output$country <- renderUI({

    if(input$gender=="w"){
      countries = countries_women
    } else{
      countries = countries_men
    }
    selectInput("country", "Country", 
                choices = countries)
  })
  
  output$exp_medalsA <- renderUI({
    team <- c(input$personA1, input$personA2, input$personA3,
              input$personA4, input$personA5) |> sort()
    if (length(unique(team)) < 5){
      div(h4("Selected team must consist of 5 unique athletes"), 
          style = "color:red;")
    } else{
      
      div(h4("Expected Medals", style = 'text-align:center;'),
          medal_row('Team', 'team', 'A'),
          medal_row('Indiv. All Around', 'aa', 'A'),
          medal_row('Indiv. Apparatus', 'event', 'A')
      )
    }
  })
  
  output$exp_medalsB <- renderUI({
    team <- c(input$personB1, input$personB2, input$personB3,
              input$personB4, input$personB5) |> sort()
    if (length(unique(team)) < 5){
      div(h4("Selected team must consist of 5 unique athletes"), 
          style = "color:red;")
    } else{
      
      div(h4("Expected Medals", style = 'text-align:center;'),
          medal_row('Team', 'team', 'B'),
          medal_row('Indiv. All Around', 'aa', 'B'),
          medal_row('Indiv. Apparatus', 'event', 'B')
      )
    }
  })
  
  ########## Compare
  
  athlete_pool <- reactive({
    weight = as.vector(outer(c(1,1,1), c(1,1,1),"*"))
    
    
    if (file.exists(paste0('../totsims/names-',paste(weight,collapse="."),
                           "-",input$country,"-",input$gender, '.csv'))){
      all_names <- read.csv(paste0('../totsims/names-',paste(weight,collapse="."),
                                   "-",input$country,"-",input$gender, '.csv')) |> 
        unlist() |> unique()
      
      all_names
    }
  })
  
  
  
observe({
    for (i in 1:5){
      updateSelectInput(inputId = paste0('personA', i), choices = athlete_pool(),
                        selected = athlete_pool()[i])
      updateSelectInput(inputId = paste0('personB', i), choices = athlete_pool(),
                        selected = athlete_pool()[i+1])
    }
  })
  
output$errorMessage <- renderUI({
  if ((input$team == 0) & (input$aa == 0) & (input$event == 0)){
    h5("Weights can not all be 0", style = "color:red;" )
  }
})
  ################## END SERVER ###############################
  
}



# Run the application 
shinyApp(ui = ui, server = server)
