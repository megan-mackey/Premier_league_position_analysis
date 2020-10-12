

library(shiny)
library(tidyverse)
# load necessary data and code from helper files, including pre-made plots and
# tables to keep the server code below as clean and concise as possible


# load text for about page

source("about_text.R")
source("league_positions.R")


# Define UI for application that draws a histogram
ui <- navbarPage(
    "Economic Consequences of league position in the Premier league",
    tabPanel("Model",
             fluidPage(
                 titlePanel("Arsenal Goals Conceded"),
                     mainPanel(plotOutput("line_plot")))
             ),
    tabPanel("Discussion",
             titlePanel("Discussion Title"),
             p("Tour of the modeling choices you made and 
              an explanation of why you made them")),
    tabPanel("About", 
             titlePanel("About"),
             h3("Project Background and Motivations"),
             p(strong(p1)),
             h3("About Me"),
             p(p2)))

# Define server logic required to draw a histogram
server <- function(input, output) {
    output$line_plot <- renderPlot({
        df_all_edit<- df_all %>%
            filter(season == "2014-2015" & team == "Arsenal")
        
        ggplot(data = df_all_edit, aes(x = num_match, y = goal_conceded)) +
            geom_col()+
            labs(title = "Goals conceded by Arsenal in 2014-2015 season", x = "Match Number", y = "goals conceded")
     
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
