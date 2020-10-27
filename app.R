library(shiny)
library(tidyverse)
library(gganimate)
library(ggplot2)
library(plotly)
# load necessary data and code from helper files, including pre-made plots and
# tables to keep the server code below as clean and concise as possible


# load text for about page

source("creator_information.R")
source("league_positions.R")
source("finance.R")



# Define UI for application that draws a histogram
ui <- navbarPage(
    "Economic Consequences of league position in the Premier league",
    tabPanel("The Premier League",
             fluidPage(
                 includeMarkdown("doc/pl.md"),
                 mainPanel(
                     plotlyOutput("season"))),
             selectInput("season", "Select a Season:",
                         choices = c(
                                     "2009-2010",
                                     "2010-2011",
                                     "2011-2012",
                                     "2012-2013",
                                     "2013-2014",
                                     "2014-2015"),
                         selected = c("2009-2010"),
                         multiple = FALSE)),
    tabPanel("Financial Consequences",
             fluidPage(
                 includeMarkdown("doc/model.md"),
                 mainPanel(
                     plotlyOutput("Money"))),
             selectInput("Money", "Select a Team:",
                         c("Arsenal",
                           "Aston Villa",
                           "Birmingham",
                           "Blackburn",
                           "Bolton ",
                           "Burnley",
                           "Chelsea",
                           "Everton",
                           "Fulham",
                           "Hull",
                           "Liverpool",
                           "Man City",
                           "Man United",
                           "Portsmouth",
                           "Stoke ",
                           "Sunderland",
                           "Tottenham",
                           "West Ham",
                           "Wigan",
                           "Wolves",
                           "Newcastle",
                           "West Brom",
                           "Blackpool",
                           "QPR",
                           "Norwich",
                           "Swansea",
                           "Reading",
                           "Southampton",
                           "Cardiff",
                           "Crystal Palace",
                           "Leicester"),
                         multiple = FALSE)),
    tabPanel("Significance of Project",
             includeMarkdown("doc/intro.md"),
    includeMarkdown("doc/source_part1.md")),
    tabPanel("Creator Information", 
             h3("Project"),
             p(p1),
             h3("Project Information"),
             p(p2)))


# Define server logic required to draw a histogram
server <- function(input, output) {
output$season <- renderPlotly ({
    
plot1 <- case_when(
                   input$season == "2009-2010" ~ list(season_2009_2010),
                   input$season == "2010-2011" ~ list(season_2010_2011),
                   input$season == "2011-2012" ~ list(season_2011_2012),
                   input$season == "2012-2013" ~ list(season_2012_2013),
                   input$season == "2013-2014" ~ list(season_2013_2014),
                   input$season == "2014-2015" ~ list(season_2014_2015),
                   TRUE ~ list(season_2009_2010)) %>%
.[[1]]
                  
})

output$Money <- renderPlotly({
    
plot_2 <- case_when(
input$Money == "Arsenal" ~ list(arsenal),
input$Money == "Aston Villa" ~ list(aston_villa),
input$Money == "Birmingham" ~ list(birmingham),
input$Money == "Blackburn" ~ list(blackburn),
input$Money == "Blackpool" ~ list(blackpool),
input$Money == "Bolton" ~ list(bolton),
input$Money == "Burnley" ~ list(burnley),
input$Money == "Cardiff" ~ list(cardiff),
input$Money == "Chelsea" ~ list(chelsea),
input$Money == "Crystal Palace" ~ list(crystal_palace),
input$Money == "Everton" ~ list(everton),
input$Money == "Fulham" ~ list(fulham), 
input$Money == "Hull" ~ list(hull),
input$Money == "Leicester" ~ list(leicester),
input$Money == "Liverpool" ~ list(liverpool),
input$Money == "Man City" ~ list(man_city),
input$Money == "Man United" ~ list(man_united),
input$Money == "Newcastle" ~ list(newcastle),
input$Money == "Norwich" ~ list(norwich),
input$Money == "Portsmouth" ~ list(portsmouth),
input$Money == "QPR" ~ list(qpr),
input$Money == "Reading" ~ list(reading),
input$Money == "Southampton" ~ list(southampton),
input$Money == "Stoke" ~ list(stoke),
input$Money == "Sunderland" ~ list(sunderland),
input$Money == "Swansea" ~ list(swansea),
input$Money == "Tottenham" ~ list(tottenham),
input$Money == "West Brom" ~ list(west_brom),
input$Money == "West Ham" ~ list(west_ham),
input$Money == "Wigan" ~ list(wigan),
input$Money == "Wolves" ~ list(wolves),
TRUE ~ list(arsenal)) %>%
    .[[1]]
})
}
# Run the application 
shinyApp(ui = ui, server = server)