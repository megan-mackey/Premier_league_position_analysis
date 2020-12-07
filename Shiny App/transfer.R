# Load libraries

library(tidyverse)
library(readxl)
library(ggplot2)
library(rstanarm)
library(stringr)
library(rsample)
library(workflows)
library(parsnip)
library(recipes)
library(yardstick)
library(tune)
library(gt)
library(readxl)

# Load data needed

load("Data/processed_data_transfer.rda")

# This is a model to show how highest transfer fee and position are linked. Similar to the 
# model looking at total payment. This allows me to see the consequences of the total payment and how
# it affects clubs ability to buy players.

set.seed(10)
transfer_split <- initial_split(transfer_final, prob = 0.80)
transfer_train <- training(transfer_split)
transfer_test  <- testing(transfer_split)
transfer_folds <- vfold_cv(transfer_train, v = 10)

transfer_wfl <- workflow() %>%
    add_model(linear_reg() %>% 
                  set_engine("lm") %>% 
                  set_mode("regression")) %>%
    add_recipe(recipe(highest_fee ~ position, 
                      data = transfer_train) %>%
                   step_dummy(all_nominal())) 


transfer_rs <- transfer_wfl %>%
    fit_resamples(resamples = transfer_folds, 
                  control = control_resamples(save_pred = TRUE)) 

predictions<- collect_predictions(transfer_rs)

predict_graph <- predictions %>% 
    ggplot(aes(x = highest_fee , y = .pred)) + 
    geom_point(alpha = .15) +
    geom_abline(col = "red") +
coord_flip() +
    labs( x = "Position", y = "Predicted",
          title = "Relationship between position and record transfer fee",
          subtitle = " Source : https://www.transfermarkt.co.uk")

# This creates a table which contains the record transfer fee paid for a player by a club.
# This provides some background information and a starting point for the viewer

highest_transfer <- read_excel("Data/transfer.xlsx") %>%
    gt()