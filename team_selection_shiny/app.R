library(shiny)
library(tidyverse)

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Team Predictions"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            selectInput("gender", "Gender", choices = c("Men", "Women")),
            radioButtons("weights",
                         "Medal weights",
                         choices = c("Maximize medals",
                                     "Maximize gold medals",
                                     "Optimize for team event",
                                     "Custom weights")),
            
            uiOutput("weight_sliders"),
        width = 6),

        # Show a plot of the generated distribution
        mainPanel(
           tableOutput("picked_team")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  # source('../get.data.r')
  # source('../prep.data.r')

  
  output$weight_sliders <- renderUI({
    categories <- c("Team Gold", "Team Silver", "Team Bronze",
                    "All-Around Gold", "All-Around Silver", "All-Around Bronze",
                    "Event Gold", "Event Silver", "Event Bronze")
    slider_list <- list()
    if (input$weights == 'Custom weights'){
      for (i in 1:9){
        slider_list[[i]] <- sliderInput(categories[i], categories[i],
                    min = 0, max = 1, value = 0.5, ticks = F)
        # slider_list[[i]] <- numericInput(categories[i], categories[i], 
        #                                  value = 0.5, min = 0, max = 1, 
        #                                  step = 0.25)
      }
      fluidPage(fluidRow(column(width = 4, slider_list[[1]]),
                         column(width = 4, slider_list[[2]]),
                         column(width = 4, slider_list[[3]])),
                fluidRow(column(width = 4, slider_list[[4]]),
                         column(width = 4, slider_list[[5]]),
                         column(width = 4, slider_list[[6]])),
                fluidRow(column(width = 4, slider_list[[7]]),
                         column(width = 4, slider_list[[8]]),
                         column(width = 4, slider_list[[9]])))
    }
    
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
