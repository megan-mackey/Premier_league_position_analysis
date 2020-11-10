library(shiny)
library(tidyverse)
library(gganimate)
library(ggplot2)
library(plotly)
library(shinythemes)
library(broom.mixed)
library(rstanarm)
library(gt)
library(tidymodels)
library(ranger)
# load necessary data and code from helper files, including pre-made plots and
# tables to keep the server code below as clean and concise as possible


# load text for pages

source("creator_information.R")
source("league_positions.R")
source("finance.R")
source("bayesian_model.R")




# Define UI for application that draws a histogram
ui <- navbarPage(
    "Economic Analysis of league positon from 2009-2015",
    tabPanel("The Premier League",
             fluidPage(
                 theme = shinytheme("flatly"),
                 includeMarkdown("doc/pl.md"),
                 includeMarkdown("doc/general_considerations.md"),
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
                 mainPanel(tabsetPanel(type = "tabs",
                                       tabPanel("Plot # 1: Team", plotlyOutput("Money")),
                                       tabPanel("Plot # 2: Duration of Model", plotlyOutput("Money_time")),
                                       tabPanel("Key Takeaway",
                                                p(textOutput("Money_text")))))),
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
                         multiple = FALSE),
             includeMarkdown("doc/general_conclusions.md")),
    tabPanel("Bayesian Model",
             fluidPage(mainPanel(tabsetPanel(type = "tabs",
                                            tabPanel("Correlation Table", gt_output("cor_table")),
                                            tabPanel("Plot", plotlyOutput("model")),
                                            tabPanel("Comparison", plotlyOutput("model_1")),
                                            tabPanel("Key Takeaway",
                                                     p(textOutput("Model_text"))))))),
    tabPanel("Significance of Project",
             includeMarkdown("doc/intro.md"),
             includeMarkdown("doc/source_part1.md")),
    tabPanel("Creator Information", 
             h3("Project"),
             p(p1),
             h3("Project Information"),
             p("If you're interested in learning more about the project or about myself, don't hesistate
                                 to reach out through email at mmackey@college.harvard.edu, or visit my", a("GitHub Account", href="https://github.com/megan-mackey/Premier_league_position_analysis"), " page.
                                 Thank you for visiting!")))




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
    
    output$Money_time <- renderPlotly({
        
        finance_final %>%
            ggplot(aes(position, `Total Payment`)) +
            geom_point() +
            facet_wrap(~ team, scales = "free_x") +
            labs(x = "Position",
                 y = "Total Payment") +
            theme_classic() +
            theme(axis.title = element_text(face = "bold")) +
            geom_smooth(method = "lm", se = FALSE, formula = y ~x)
    })
  
    output$Money_text <- renderText({
"These are a few of the major themes which have come from comparing the model above, these are not limited but included:\n
1. Staying in the Premier League is so important. Teams who have the odd season bad season and are still in the league are able to recover due to the cumulative income received over time. Teams who only stay in the league for one season such as Portsmouth, are heavily reliant on the money received from the last season in the Championship, they do not have the economic power to cause much change in league position such as buy the best players out there.\n
2. Teams lower down in the table rely more on equal share payments as they receive less merit payments and facility fees. \n
3. Teams who stay in the Premier league for longer have increased facility fees over time. This is attributed to increased global exposure, more people are wanting to watch them play and hence more games are broadcasted. This is extra income for those teams.\n
4. As well as staying in the league, consistency in position is key. Flucuations seen in many teams lower down the table means teams cannot prepare for the upcoming seasons and if they are lucky to stay in the league, they cannot plan how to improve until season is finished. So finishing in a position which is higher or the same as before can be important.\n
5. Teams at the end of the model receive more money in general no matter what positon the team finishes in. This means for some teams it is even more important to stay in the league as the gap between money the league grows. \n
6. There is less one season teams the further into the model you go. This causes us to question the teams and their management of their cumulative income of previous seasons. But also, teams could perform better knowing what is at risk if they get relegated."
    })  
    
output$cor_table <- render_gt({   
cor_table
  
  
  
})

output$model <- renderPlotly({
  set.seed(10)
  
  finance_split <- initial_split(finance_final, prob = 0.80)
  
  finance_train <- training(finance_split)
  finance_test  <-  testing(finance_split)
  
  
  finance_rec <- 
    recipe(`Total Payment` ~ position + team,
           data = finance_train) %>%
    step_dummy(all_nominal())
  
  model <- ggplot(finance_train, aes(x = position, y = `Total Payment`)) + 
    geom_point(alpha = .2) + 
    geom_smooth(method = lm, formula = y ~ x, se = FALSE, col = "red") + 
    scale_x_log10() + 
    scale_y_log10() + 
    labs(x = "Position in league", y = "Total Payment in millions (GBP)") +
    scale_x_continuous(breaks = c(1,5,10,15,20)) +
    labs(title = " Relationship between position and total payment")


})

output$model_1 <- renderPlotly({
  comp_model <- stan_glm(data = finance_final, 
                         formula = `Total Payment` ~ position + team,
                         refresh = 0)
  
  comp_model %>%
    as_tibble() %>%
    mutate(manu_payment = `(Intercept)` + `teamMan United`) %>%
    mutate(tottenham_payment = `(Intercept)` + teamTottenham) %>%
    select(manu_payment, tottenham_payment) %>% 
    pivot_longer(cols = manu_payment:tottenham_payment, 
                 names_to = "Team",
                 values_to = "payment") %>% 
    ggplot(aes(payment, fill = Team)) +
    geom_histogram(aes(y = after_stat(count/sum(count))),
                   alpha = 0.5, 
                   bins = 100, 
                   position = "identity") +
    labs(title = "Posterior Probability Distribution",
         subtitle = "Comparing Tottenham and Man United",
         x = "Average  Payment received",
         y = "Probability") + 
    scale_x_continuous(labels = scales::number_format()) +
    scale_y_continuous(labels = scales::percent_format()) +
    theme_classic() +
    scale_fill_discrete(labels = c("Man United", "Tottenham"))

})

    
    
}
# Run the application 
shinyApp(ui = ui, server = server)