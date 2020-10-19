library(shiny)
library(tidyverse)
library(gganimate)
# load necessary data and code from helper files, including pre-made plots and
# tables to keep the server code below as clean and concise as possible


# load text for about page

source("about_text.R")
source("league_positions.R")


# Define UI for application that draws a histogram
ui <- navbarPage(
    "Economic Consequences of league position in the Premier league",
    tabPanel("League Position",
             fluidPage(
                 titlePanel("League Position in a season"),
                 mainPanel(plotOutput("line_plot"))),
    selectInput("season", "Select a Season:",
                choices = c("2005-2006",
                            "2006-2007",
                            "2007-2008",
                            "2008-2009",
                            "2009-2010",
                            "2010-2011",
                            "2011-2012",
                            "2012-2013",
                            "2013-2014",
                            "2014-2015"),
                selected = c("2005-2006"),
                multiple = FALSE)),
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
        df_all %>%
            filter(season == input$season) %>%
            
            ggplot(aes(x = num_match , y = position, color = team)) +
            geom_line()+
            labs(title = "Change in position over the selected season", 
                 x = " Number of match", y = "League Position") +
            transition_reveal(num_match)
    })
}

# Run the application 
shinyApp(ui = ui, server = server)