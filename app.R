library(shiny)
library(tidyverse)
library(gganimate)
library(ggplot2)
library(plotly)
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
                 mainPanel(
                     plotlyOutput("season"))),
             selectInput("season", "Select a Season:",
                         choices = c(
                                     "2005-2006",
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
    tabPanel("Significance of Project",
             includeMarkdown("intro.md"),
    includeMarkdown("source_part1.md")),
    tabPanel("Creator Information", 
             h3("Project"),
             p(p1),
             h3("Project Information"),
             p(p2)))

# Define server logic required to draw a histogram
server <- function(input, output) {
output$season <- renderPlotly ({
    
plot1 <- case_when(
                   input$season == "2005-2006" ~ list(season_2005_2006),
                   input$season == "2006-2007" ~ list(season_2006_2007),
                   input$season == "2007-2008" ~ list(season_2007_2008),
                   input$season == "2008-2009" ~ list(season_2008_2009),
                   input$season == "2009-2010" ~ list(season_2009_2010),
                   input$season == "2010-2011" ~ list(season_2010_2011),
                   input$season == "2011-2012" ~ list(season_2011_2012),
                   input$season == "2012-2013" ~ list(season_2012_2013),
                   input$season == "2013-2014" ~ list(season_2013_2014),
                   input$season == "2014-2015" ~ list(season_2014_2015),
                   TRUE ~ list(season_2004_2005)) %>%
.[[1]]
                  
})
}
# Run the application 
shinyApp(ui = ui, server = server)