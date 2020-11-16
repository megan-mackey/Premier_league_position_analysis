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
    labs( x = "Position", y = "Record Transfer Fee (Millions of Pounds)",
          title = "Relationship between position and record transfer fee",
          subtitle = " Source : https://www.transfermarkt.co.uk")

summary_table <- highest_transfer %>%
    rename(`Transfer Record (Millions of Pounds)` = highest_fee,
           Team = team) %>%
    gt()