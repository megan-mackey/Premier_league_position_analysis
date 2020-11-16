library(tidyverse)
library(ggplot2)

load("Data/teams.rda")



# Bolton
bolton <- finance_final %>%
  filter(team == "Bolton") %>%
  ggplot(aes(x = season, y = Finances, fill = type))+
  geom_col(position = "stack")+
  geom_point(aes(x = season, y = position))+
  labs(title = "Amount of Money received by Bolton",
       x = "Season",
       y = "Money Received ( Millions of Pounds)") +
  scale_fill_discrete(name = "Type")

# Stoke
stoke <-  finance_final %>%
  filter(team == "Stoke") %>%
  ggplot(aes(x = season, y = Finances, fill = type))+
  geom_col(position = "stack")+
  geom_point(aes(x = season, y = position)) +
  labs(title = "Amount of Money received by Stoke",
       x = "Season",
       y = "Money Received ( Millions of Pounds)") +
  scale_fill_discrete(name = "Type")
