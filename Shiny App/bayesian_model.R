# Load libraries

library(tidyverse)
library(tidymodels)
library(ranger)
library(gtsummary)

load("Data/processed_data_finance_edit.rda")

# Load data

set.seed(10)

finance_split <- initial_split(finance_final, prob = 0.80)

finance_train <- training(finance_split)
finance_test  <-  testing(finance_split)
finance_folds <- vfold_cv(finance_final, folds = 5)

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

# This is already in the app.r file but includes the model for the app, the total payment and position

comp_model_1 <- stan_glm(data = finance_final, 
                       formula = `Total Payment` ~ position,
                       refresh = 0)

# This allows us to create a gt table with confidence interval just for position

cor_table <- tbl_regression(comp_model_1, exponentiate = TRUE) %>%
  modify_table_header(column = estimate,
                      label = "**Beta**") %>%
  as_gt() %>%
  tab_header(title  = "Regression of Total Payment",
             subtitle = "The Effect of position on money recieved") 

# This shows the correlation between the recipe used in the model. The correlation is displayed in a gt table


comp_model <- stan_glm(data = finance_final, 
                       formula = `Total Payment` ~ position + team,
                       refresh = 0)

# I used stan_glm to create  a posterior distribution of the two teams which I am comparing (Man United and Tottenham)

comp_graph <- comp_model %>%
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
       x = "Average  Payment received (Millions of Pounds)",
       y = "Probability") + 
  scale_x_continuous(labels = scales::number_format()) +
  scale_y_continuous(labels = scales::percent_format()) +
  theme_classic() +
  scale_fill_discrete(labels = c("Man United", "Tottenham"))

# After fitting the model, I then created graph to show this. I needed to use the intercept
# to create the man united payment and the same for Tottenham. I then pivoted longer because 
# without it, the format means I would not be able to graph it. Using position = identity means
# I can plot both teams side by side and compare.

# In order to see if the data fits well we can find the RMSE value.

finance_rec <- workflow() %>%
  add_model(linear_reg() %>% 
              set_engine("lm") %>% 
              set_mode("regression")) %>%
  add_recipe(recipe(`Total Payment` ~ position + team,
                    data = finance_train) %>%
               step_dummy(all_nominal()))%>%
  
  fit_resamples(resamples = finance_folds,
                control = control_resamples(save_pred = TRUE)) %>%
  collect_metrics()

# There is a low rmse value which is more desirable because this shows the data fits well and 
# there is a higher accuracy

