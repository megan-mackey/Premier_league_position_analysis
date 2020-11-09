library(shiny)
library(gt)
library(rstanarm)
library(broom.mixed)

finance_model <- stan_glm(data = finance_final,
                      formula = `Total Payment` ~ position,
                      refresh = 0)

tbl_regression(finance_model, exponentiate = TRUE) %>%
  modify_table_header(column = estimate,
                      label = "**Beta**") %>%
  as_gt() %>%
  tab_header(title  = "Regression of Total Payment",
             subtitle = "The Effect of position finish in Premier League")