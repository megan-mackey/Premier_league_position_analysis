
# load necessary libraries

library(shiny)
library(tidyverse)
library(ggplot2)
library(plotly)
library(shinythemes)
library(broom.mixed)
library(rstanarm)
library(gt)
library(tidymodels)
library(ranger)
library(shinycustomloader)
library(rmarkdown)
library(readxl)
library(stringr)
library(rsample)
library(workflows)
library(parsnip)
library(recipes)
library(yardstick)
library(tune)
library(readr)
library(gtsummary)
# load necessary data and code from helper files, including pre-made plots and
# tables to keep the server code below as clean and concise as possible


# load text for pages

source("creator_information.R")
source("finance.R")
source("bayesian_model.R")
source("transfer.R")


# Create UI

ui <- navbarPage(
    "How important is league finish in the English Premier League",
    tabPanel("The Premier League",
             fluidPage(
                 mainPanel(
                 imageOutput("season"), type="html", loader="loader2"),
                 
# The image output function allows for me to include a GIF, which is a animated
# graph showing the league positions of teams in the model.
                 
                 includeMarkdown("doc/pl.md"),
                 
# Load in a markdown file as it allows me to include images and to de-clutter
# the ui. This markdown includes information related to introducing the topic.
                 
                 p(h3("Please see the general considerations", a("here.",
                                                              href="https://docs.google.com/document/d/1zs9XcNMXfMh1g4XEWCugf-kLEbzam0eXWQnIbGknLco/edit#"))))),

# Instead of having text and an URL, I embedded a hyperlink which cleans up the aesthetic

    tabPanel("Financial Consequences",
             fluidPage(
                 mainPanel(tabsetPanel(type = "tabs",
                                       tabPanel("Plot # 1: Team", plotlyOutput("Money")),
                                       tabPanel("Plot # 2: Duration of Model", plotlyOutput("Money_time")),
                                       tabPanel("Key Takeaways",
                                                p(textOutput("Money_text")))))),
             
# In order to make the aesthetic look neater, I divided the information into tabs. The first tab, show the 
# individual teams and their total payment over the course of the model. The second tab is all the teams in
# the model. This is just total payment and not the 3 individual categories. The final tab, shows key takeaways
# which we might draw from the previous two graphs.            
             
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


# The select input includes all the teams which have been present in the premier league from 2009 to 2015

             includeMarkdown("doc/model.md")),

# This model markdown defines some of the terminology used in the graph and finds some basic background information
# to add understanding

    tabPanel("Model",
             fluidPage(mainPanel(tabsetPanel(type = "tabs",
                                            tabPanel("Correlation Table",
                                                     p("In order to find the average position of each team in the middle you add the position parameter to the value associated with the team"),
                                                       gt_output("cor_table")),
                                            tabPanel("Plot",
                                                          plotlyOutput("model")),
                                            tabPanel("Comparison", 
                                                     p("Comparing two teams who withstood relegation and stayed in the model for the 6 seasons. This will make it easier to compare as they have equivalent data points. On average Man United finished in 3rd place and Tottenham 5th."),
                                                     plotlyOutput("model_1")),
                                            tabPanel("Key Takeaway",
                                                     p(textOutput("Model_text"))))))),

# Again using tabs, the first tab is the correlation table for the model. The second tab is the model graphed showing
# the relationship between total payment and position. The third tab again shows some key conclusions which we can be
# deduced.

    tabPanel("Consequence: Transfers",
             fluidPage(mainPanel(tabsetPanel(type = "tabs",
                                             tabPanel("Record Transfer Fees per team",
                                                      gt_output("transfer_table")),
                                             tabPanel("Plot",
                                                      plotlyOutput("transfers")),
                                             tabPanel("Key Takeaway",
                                                      p(textOutput("model_transfer"))))))),

# In order to see the significance of total payment and the consequences of it, we can show the highest record
# transfer. Each team's record transfer can be seen on tab 1 , on tab 2 is the model showing the relationship between position
# and record transfer. The final tab is the key takeaways.

    tabPanel("Significance of Project",
             includeMarkdown("doc/intro.md")),
             
# This markdown shows the significance of the projects and answers the 5 w's: who, what, where, when and why in regard to the project

# This markdown shows the data sources I used and provides acknowledgments too.

    tabPanel("About", 
            h3("Project"),
           p(p1),
           p(p2),
           p(p3),
           p(p4),
           
# These are objects from the creator.information.R file, this shows information about some basic conclusions
# on a whole we can draw from all the information discussed.
  
includeMarkdown("doc/source_part1.md"),
h3("Project Information"),
             p("If you're interested in learning more about the project or about myself, don't hesistate
                                 to reach out through email at megan.mackey01@gmail.com, or visit my", a("GitHub Account", href="https://github.com/megan-mackey"), " page.
                                 Thank you for visiting!")))


# This markdown shows the data sources I used and provides acknowledgments too.


# This provides some basic contact information for myself and then includes a hyperlink to my github account.




# Create server
server <- function(input, output) {
    output$season <- renderImage ({

   list(
  src = './Gifs/edit_gif.gif',
  contentType = 'image/gif',
  width = 700,
  height = 500
)
      
# This draws on the image output used in the UI, to input the gif into the shiny app.      

      
    }, deleteFile = F)
  
    
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

# I used a case_when which will apply to the drop down menu, it draws on the team name objects
# created in the finance.R file. If selected in the drop down menu, this graph will be displayed.

    
    output$Money_time <- renderPlotly({
        
        finance_final %>%
            ggplot(aes(position, `Total Payment`)) +
            geom_point() +
            facet_wrap(~ team, scales = "free_x") +
            labs(x = "Position",
                 y = "Total Payment") +
            theme_classic() +
            theme(axis.title = element_text(face = "bold")) +
            geom_smooth(method = "lm", se = FALSE, formula = y ~ x)
    })
    
# This provides just the total payment but using scales allows us to compare all the teams at once.
  
    output$Money_text <- renderText({

" Teams who have the odd season bad season and are still in the league are able to recover due to the cumulative income received over time.
Teams lower down in the table rely more on equal share payments as they receive less merit payments and facility fees.
Teams who stay in the Premier league for longer have increased facility fees over time. This is attributed to increased global exposure, more people are wanting to watch them play and hence more games are broadcasted. This is extra income for those teams.
Consistency in position is key. Flucuations seen in many teams lower down the table means teams cannot prepare for the upcoming season. 
Teams at the end of the model receive more money in general no matter what positon the team as there was increase investment in league.
There is less one season teams the further into the model you go. This causes us to question the teams and their management of their cumulative income of previous seasons. But also, teams could perform better knowing what is at risk if they get relegated."
    })  
    
# This is the text which will be displayed in the key takeaway tab.
    
output$cor_table <- render_gt({   
cor_table
  
# You can find this object cor_table in bayesian_model.r file. This shows the correlation between
# total payment and position
  
})

output$model <- renderPlotly({
  set.seed(8)
  finance_split <- initial_split(finance_final, prob = 0.80)
  
  finance_train <- training(finance_split)
  finance_test  <-  testing(finance_split)
  finance_folds <- vfold_cv(finance_final, folds = 7)
  
  finance_rec <- workflow() %>%
    add_model(linear_reg() %>% 
                set_engine("stan") %>% 
                set_mode("regression")) %>%
    add_recipe(recipe(`Total Payment` ~ position + team,
                      data = finance_train) %>%
                 step_dummy(all_nominal()))
  
  finance_rec %>%
    fit(data = finance_train) %>%
    predict(new_data = finance_test) %>% 
    bind_cols(finance_test %>% select(`Total Payment`)) %>%
    
    ggplot(aes(x = `Total Payment`, y = .pred)) + 
    geom_point(alpha = .2) + 
    geom_smooth(method = lm, formula = y ~ x, se = FALSE, col = "red") + 
    scale_x_log10() + 
    scale_y_log10() + 
    labs(y = "Predicted", x = "Total Payment in millions (GBP)") +
    labs(title = " Relationship between position and total payment")


})

# This shows our model, first we set the data on the training data, then we predict on the testing data
# and plot.

output$model_1 <- renderPlotly({
  comp_graph 
  
# This object can be found on the bayesian_model.r file. It displays the comparison between two teams, Man United and 
  # Tottenham, both been in the model for the whole duration yet had different average positions.
  
})

output$Model_text <- renderText ({
" The relationship between total payment and position show a negative correlation. In other words, the lower the position the less money you will recieve. Also,
  the difference between two positions is significant (comparison tab). The median value for Tottenham is £51 million pounds and for Man Utd £55 million pounds.
  On average they are only two positions different but in regard to total payment the difference is £4 million significant. Shows why teams are so keen to finish even 1 position 
  higher than the previous season. " 
  
# This is the text which will appear under the key takeaway tab.  
  
})  

output$transfer_table <- render_gt({
highest_transfer
  
})


# This object is found in the transfer.R file, which shows the highest record transfer for each team in the model

output$transfers <-renderPlotly({
predict_graph
  
})

# This object is found in the transfer.R file and displays the relationship between highest transfer and position.

output$model_transfer <- renderText({
"Where you finish in the league determines the amount of money you receive, from the model here we can see
that there is a correlation between position and record transfer fee. The lower the position, the lower the record
  transfer fee. A team's record transfer fee can be seen as a consequence of the total payment they recieve." 
  
  
})

# This will be displayed under key takeaway tab
    
}
# Run the application 
shinyApp(ui = ui, server = server)