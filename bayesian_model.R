library(tidyverse)
library(tidymodels)
library(ranger)
data("processed_data_finance")

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


cor_table <- finance_final %>% 
  group_by(season) %>% 
  summarize(cor = cor(position, `Total Payment`, use = "complete.obs"), .groups = "drop") %>%
  mutate(cor = round(cor, digits = 2)) %>% 
  gt() %>%
  cols_label(season = md("**Season**"),
             cor = md("**Correlation Coefficient**")) %>%
  cols_align(columns = "season", align = "left") %>% 
  tab_options(container.height = 535) %>% 
  tab_header(title = "Position and Total Payment per season",
             subtitle = "Strength of Relationship by Season")
