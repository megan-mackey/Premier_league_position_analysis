
library(tidyverse)
library(ggplot2)


load("Data/processed_data.rda")

# Plot League Position of Manchester United over 2014-2015 season

df_all_edit<- df_all %>%
filter(season == "2014-2015" & team == "Arsenal")

ggplot(data = df_all_edit, aes(x = num_match, y = goal_conceded)) +
geom_col()+
labs(title = "Goals conceded by Arsenal in 2014-2015 season", x = "Match Number", y = "goals conceded")

